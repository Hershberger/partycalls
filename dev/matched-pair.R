library(partycalls)
load("test_data/senate_data_lm.RData")
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, y = pirate100)]
setorder(DATA, stabb, congress, class)
DATA[congress == 107 & caucus == "Democrat", maj := 1]
DATA[congress == 107 & caucus == "Republican", maj := 0]
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, mean_tr := mean(tr), .(stabb, congress)]
DATA[, rand := runif(nrow(DATA))]

DATA[, both_same_party := 1 * (length(unique(caucus)) == 1), .(stabb, congress)]
DATA[, both_democrats := 0]
DATA[, both_republicans := 0]
DATA[both_same_party == 1, both_democrats := 1 * (unique(caucus)[1] == "Democrat"), .(stabb, congress)]
DATA[both_same_party == 1, both_republicans := 1 * (unique(caucus)[1] == "Republican"), .(stabb, congress)]
DATA[, both_same_majority_status := 1 * (length(unique(maj)) == 1), .(stabb, congress)]
DATA[, both_majority := 0]
DATA[, both_minority := 0]
DATA[both_same_majority_status == 1, both_majority := 1 * (unique(maj)[1] == 1), .(stabb, congress)]
DATA[both_same_majority_status == 1, both_minority := 1 * (unique(maj)[1] == 0), .(stabb, congress)]
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
set.seed(30192873)
boots <- rbindlist(lapply(1:1000, boot))
c(effect, boots[, quantile(boot_effect, c(.025, .975))])
c(placebo, boots[, quantile(boot_placebo, c(.025, .975))])


# Estimate Effects Adjusted for seat_pair_type
n_obs_in_treated_pairs <- nrow(DATA[mean_tr == .5])
n_treated_pairs <- n_obs_in_treated_pairs / 2
effect <- DATA[mean_tr == .5,
  sum(tr * y) - sum((1 - tr) * y), .(stabb, congress, seat_pair_type)][,
    .(conditional_effect = mean(V1), weight = .N / n_treated_pairs),
    seat_pair_type][,
      sum(conditional_effect * weight)]
n_obs_in_placebo_pairs <- nrow(DATA[mean_tr == 0])
n_placebo_pairs <- n_obs_in_placebo_pairs / 2
placebo <- DATA[mean_tr == 0,
  sum((rand > mean(rand)) * y) - sum((rand < mean(rand)) * y),
  .(stabb, congress, seat_pair_type)][,
    .(conditional_effect = mean(V1), weight = .N / n_placebo_pairs),
    seat_pair_type][,
      sum(conditional_effect * weight)]

# put bootstrapping here
