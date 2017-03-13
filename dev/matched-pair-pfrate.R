library(partycalls)
library(xtable)
set.seed(98037298)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pfrate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, y = pfrate100)]
setorder(DATA, stabb, congress, class)

# make dems majority for congress 107
DATA[congress == 107 & caucus == "Democrat", maj := 1]
DATA[congress == 107 & caucus == "Republican", maj := 0]

# subset to cases with two senators, one treated, one control
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, mean_tr := mean(tr), .(stabb, congress)]
DATA[, rand := runif(nrow(DATA))]

# define types of cases by party/majority makeup
DATA[, both_same_party := 1 * (length(unique(caucus)) == 1), .(stabb, congress)]
DATA[, both_democrats := 0]
DATA[, both_republicans := 0]
DATA[both_same_party == 1, both_democrats := 1 * (unique(caucus)[1] == "Democrat"),
  .(stabb, congress)]
DATA[both_same_party == 1, both_republicans := 1 * (unique(caucus)[1] == "Republican"),
  .(stabb, congress)]
DATA[, both_same_majority_status := 1 * (length(unique(maj)) == 1), .(stabb, congress)]
DATA[, both_majority := 0]
DATA[, both_minority := 0]
DATA[both_same_majority_status == 1, both_majority := 1 * (unique(maj)[1] == 1),
  .(stabb, congress)]
DATA[both_same_majority_status == 1, both_minority := 1 * (unique(maj)[1] == 0),
  .(stabb, congress)]
DATA[, majority_democrat := 1 * (maj == 1 & caucus == "Democrat")]
DATA[, majority_republican := 1 * (maj == 1 & caucus == "Republican")]
DATA[, split_majority_democrat := 0]
DATA[, split_majority_republican := 0]
DATA[both_same_party == 0 & both_same_majority_status == 0,
  split_majority_democrat := 1 * (max(majority_democrat) == 1),
  .(stabb, congress)]
DATA[both_same_party == 0 & both_same_majority_status == 0,
  split_majority_republican := 1 * (max(majority_republican) == 1),
  .(stabb, congress)]
DATA[both_majority == 1 & both_democrats == 1, seat_pair_type := "2 maj dems"]
DATA[both_majority == 1 & both_republicans == 1, seat_pair_type := "2 maj reps"]
DATA[both_minority == 1 & both_democrats == 1, seat_pair_type := "2 min dems"]
DATA[both_minority == 1 & both_republicans == 1, seat_pair_type := "2 min reps"]
DATA[split_majority_democrat == 1, seat_pair_type := "split/maj dem"]
DATA[split_majority_republican == 1, seat_pair_type := "split/maj rep"]

DATA[, `:=`(both_same_party = NULL, both_democrats = NULL,
  both_republicans = NULL, both_same_majority_status = NULL,
  both_majority = NULL, both_minority = NULL, majority_democrat = NULL,
  majority_republican = NULL, split_majority_democrat = NULL,
  split_majority_republican = NULL)]

# Estimate Effects
effect <- DATA[mean_tr == .5,
  sum(tr * y) - sum((1 - tr) * y), .(stabb, congress)][,
    mean(V1)]
placebo <- DATA[mean_tr == 0,
  sum((rand > mean(rand)) * y) - sum((rand < mean(rand)) * y),
  .(stabb, congress)][,
    mean(V1)]

# Do inference
# bootstrap by state
states <- DATA[, unique(stabb)]

boot <- function(i) {
  boot_states <- sample(states, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- DATA[stabb == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_effect <- boot_DATA[mean_tr == .5,
    sum(tr * y) - sum((1 - tr) * y), .(boot_id, congress)][,
      mean(V1)]
  boot_DATA[, rand := runif(nrow(boot_DATA))]
  boot_placebo <- boot_DATA[mean_tr == 0,
    sum((rand > mean(rand)) * y) - sum((rand < mean(rand)) * y),
    .(boot_id, congress)][,
      mean(V1)]
  data.table(boot_effect, boot_placebo)
}

boots <- rbindlist(lapply(1:1000, boot))
naive_difference <- data.table(test = c("Effect", "Placebo"),
  DV = "pfrate100",
  Estimate = c(effect, placebo),
  Lower_Bound = c(boots[, quantile(boot_effect, .025)],
    boots[, quantile(boot_placebo, .025)]),
  Upper_Bound = c(boots[, quantile(boot_effect, .975)],
    boots[, quantile(boot_placebo, .975)])
)

# Estimate Effects Adjusted for seat_pair_type
n_obs_in_treated_pairs <- nrow(DATA[mean_tr == .5])
n_treated_pairs <- n_obs_in_treated_pairs / 2
effect_adjusted <- DATA[mean_tr == .5,
  sum(tr * y) - sum((1 - tr) * y), .(stabb, congress, seat_pair_type)][,
    .(conditional_effect = mean(V1), weight = .N / n_treated_pairs),
    seat_pair_type][,
      sum(conditional_effect * weight)]
n_obs_in_placebo_pairs <- nrow(DATA[mean_tr == 0])
n_placebo_pairs <- n_obs_in_placebo_pairs / 2
placebo_adjusted <- DATA[mean_tr == 0,
  sum((rand > mean(rand)) * y) - sum((rand < mean(rand)) * y),
  .(stabb, congress, seat_pair_type)][,
    .(conditional_effect = mean(V1), weight = .N / n_placebo_pairs),
    seat_pair_type][,
      sum(conditional_effect * weight)]

# put bootstrapping here
boot_adjusted <- function(i) {
  boot_states <- sample(states, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- DATA[stabb == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_effect_adjusted <- boot_DATA[mean_tr == .5,
    sum(tr * y) - sum((1 - tr) * y), .(stabb, congress, seat_pair_type)][,
      .(conditional_effect = mean(V1), weight = .N / n_treated_pairs),
      seat_pair_type][,
        sum(conditional_effect * weight)]
  boot_DATA[, rand := runif(nrow(boot_DATA))]
  placebo_adjusted <- boot_DATA[mean_tr == 0,
    sum((rand > mean(rand)) * y) - sum((rand < mean(rand)) * y),
    .(stabb, congress, seat_pair_type)][,
      .(conditional_effect = mean(V1), weight = .N / n_placebo_pairs),
      seat_pair_type][,
        sum(conditional_effect * weight)]
  data.table(boot_effect_adjusted, placebo_adjusted)
}

adjusted_boots <- rbindlist(lapply(1:1000, boot_adjusted))


# get output
seat_type_effect <- DATA[mean_tr == .5, sum(tr * y) - sum((1 - tr) * y),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_effect, "V1", "effect")
seat_type_effect

seat_type_placebo <- DATA[, sum((rand > mean(rand)) * y) - sum(((rand) <= mean(rand)) * y),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_placebo, "V1", "placebo")
seat_type_placebo

adjusted_difference <- data.table(
  Test = c("Adjusted Effect", "Adjusted Placebo"),
  DV = "pfrate100",
  estimate = c(effect_adjusted, placebo_adjusted),
  Lower_Bound = c(adjusted_boots[, quantile(boot_effect_adjusted, .025)],
    adjusted_boots[, quantile(placebo_adjusted, .025)]),
  Upper_Bound = c(adjusted_boots[, quantile(boot_effect_adjusted, .975)],
    adjusted_boots[, quantile(placebo_adjusted, .975)])
)

seat_type_difference <- data.table(
  Test = c("2 Maj Dems Effect", "2 Maj Dems Placebo",
    "2 Min Dems Effect", "2 Min Dems Placebo",
    "2 Maj Reps Effect", "2 Maj Reps Placebo",
    "2 Min Reps Effect", "2 Min Reps Placebo",
    "Split, Maj Dem Effect", "Split, Maj Dem Placebo",
    "Split, Maj Rep Effect", "Split, Maj Rep Placebo"),
  DV = "pfrate100",
  Estimate = c(
    seat_type_effect[seat_pair_type == "2 maj dems", effect],
    seat_type_placebo[seat_pair_type == "2 maj dems", placebo],
    seat_type_effect[seat_pair_type == "2 min dems", effect],
    seat_type_placebo[seat_pair_type == "2 min dems", placebo],
    seat_type_effect[seat_pair_type == "2 maj reps", effect],
    seat_type_placebo[seat_pair_type == "2 maj reps", placebo],
    seat_type_effect[seat_pair_type == "2 min reps", effect],
    seat_type_placebo[seat_pair_type == "2 min reps", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep", placebo])
)

naive_difference_tex <- xtable(naive_difference, auto = TRUE)
print(naive_difference_tex, include.rownames = FALSE)
adjusted_difference_tex <- xtable(adjusted_difference, auto = TRUE)
print(adjusted_difference_tex, include.rownames = FALSE)
seat_type_difference_tex <- xtable(seat_type_difference, auto = TRUE)
print(seat_type_difference_tex, include.rownames = FALSE)
