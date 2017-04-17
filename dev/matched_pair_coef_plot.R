library(partycalls)
library(xtable)
set.seed(98037298)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  seniority, tr = up_for_reelection, y1 = pirate100, y2 = pfrate100)]
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

DATA[seat_pair_type == "split/maj dem" & caucus == "Democrat" & tr == 1,
  seat_pair_type := "split/maj dem, dem"]
DATA[seat_pair_type == "split/maj dem" & caucus == "Republican" & tr == 1,
  seat_pair_type := "split/maj dem, rep"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Democrat" & tr == 1,
  seat_pair_type := "split/maj rep, dem"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Republican" & tr == 1,
  seat_pair_type := "split/maj rep, rep"]

DATA[seat_pair_type == "split/maj dem" & caucus == "Democrat" & tr == 0,
  seat_pair_type := "split/maj dem, rep"]
DATA[seat_pair_type == "split/maj dem" & caucus == "Republican" & tr == 0,
  seat_pair_type := "split/maj dem, dem"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Democrat" & tr == 0,
  seat_pair_type := "split/maj rep, rep"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Republican" & tr == 0,
  seat_pair_type := "split/maj rep, dem"]

# remove unneeded variables
DATA[, `:=`(both_same_party = NULL, both_democrats = NULL,
  both_republicans = NULL, both_same_majority_status = NULL,
  both_majority = NULL, both_minority = NULL, majority_democrat = NULL,
  majority_republican = NULL, split_majority_democrat = NULL,
  split_majority_republican = NULL)]

# Estimate Effects
effect_pi <- DATA[mean_tr == .5,
  sum(tr * y1) - sum((1 - tr) * y1), .(stabb, congress)][,
    mean(V1)]
placebo_pi <- DATA[mean_tr == .5,
  sum((seniority > mean(seniority)) * y1) - sum((seniority < mean(seniority)) * y1),
  .(stabb, congress)][,
    mean(V1)]

effect_pf <- DATA[mean_tr == .5,
  sum(tr * y2) - sum((1 - tr) * y2), .(stabb, congress)][,
    mean(V1)]
placebo_pf <- DATA[mean_tr == 0,
  sum((seniority > mean(seniority)) * y2) - sum((seniority < mean(seniority)) * y2),
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
  boot_effect_pi <- boot_DATA[mean_tr == .5,
    sum(tr * y1) - sum((1 - tr) * y1), .(boot_id, congress)][,
      mean(V1)]
  boot_placebo_pi <- boot_DATA[mean_tr == 0,
    sum((seniority > mean(seniority)) * y1) - sum((seniority < mean(seniority)) * y1),
    .(boot_id, congress)][,
      mean(V1)]
  boot_effect_pf <- boot_DATA[mean_tr == .5,
    sum(tr * y2) - sum((1 - tr) * y2), .(boot_id, congress)][,
      mean(V1)]
  boot_placebo_pf <- boot_DATA[mean_tr == 0,
    sum((seniority > mean(seniority)) * y2) - sum((seniority < mean(seniority)) * y2),
    .(boot_id, congress)][,
      mean(V1)]

  data.table(boot_effect_pi, boot_placebo_pi, boot_effect_pf, boot_placebo_pf)
}

boots <- rbindlist(lapply(1:1000, boot))
naive_difference <- data.table(test = c("Effect", "Placebo", "Effect", "Placebo"),
  DV = c("pirate100", "pirate100", "pfrate100", "pfrate100"),
  Estimate = c(effect_pi, placebo_pi, effect_pf, placebo_pf),
  Lower_Bound = c(boots[, quantile(boot_effect_pi, .025)],
    boots[, quantile(boot_placebo_pi, .025)],
    boots[, quantile(boot_effect_pf, .025)],
    boots[, quantile(boot_placebo_pf, .025)]),
  Upper_Bound = c(boots[, quantile(boot_effect_pi, .975)],
    boots[, quantile(boot_placebo_pi, .975)],
    boots[, quantile(boot_effect_pf, .975)],
    boots[, quantile(boot_placebo_pf, .975)])
)


seat_type_effect <- DATA[mean_tr == .5, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_effect, "V1", "effect")

seat_type_placebo <- DATA[, sum((seniority > mean(seniority)) * y1) -
    sum(((seniority) <= mean(seniority)) * y1),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_placebo, "V1", "placebo")

seat_type_difference <- data.table(
  Test = c("2 Maj Dems Effect", "2 Maj Dems Placebo",
    "2 Min Dems Effect", "2 Min Dems Placebo",
    "2 Maj Reps Effect", "2 Maj Reps Placebo",
    "2 Min Reps Effect", "2 Min Reps Placebo",
    "Split, Maj Dem, Dem Effect", "Split, Maj Dem, Dem Placebo",
    "Split, Maj Dem, Rep Effect", "Split, Maj Dem, Rep Placebo",
    "Split, Maj Rep, Dem Effect", "Split, Maj Rep, Dem Placebo",
    "Split, Maj Rep, Rep Effect", "Split, Maj Rep, Rep Placebo"),
  DV = "pirate100",
  Estimate = c(
    seat_type_effect[seat_pair_type == "2 maj dems", effect],
    seat_type_placebo[seat_pair_type == "2 maj dems", placebo],
    seat_type_effect[seat_pair_type == "2 min dems", effect],
    seat_type_placebo[seat_pair_type == "2 min dems", placebo],
    seat_type_effect[seat_pair_type == "2 maj reps", effect],
    seat_type_placebo[seat_pair_type == "2 maj reps", placebo],
    seat_type_effect[seat_pair_type == "2 min reps", effect],
    seat_type_placebo[seat_pair_type == "2 min reps", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem, dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem, dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem, rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem, rep", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep, dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep, dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep, rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep, rep", placebo]
  )
)

naive_difference_tex <- xtable(naive_difference, auto = TRUE,
  caption = "Reelection and Response to Party Calls, Difference in Differences",
  digits = c(3, 3, 3, 3, 3, 3))
print(naive_difference_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")

seat_type_difference_tex <- xtable(seat_type_difference, auto = TRUE,
  caption = "Diff in Diff, Subgroup Condition, Party Influenced Rate")
print(seat_type_difference_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")

# make coeff plot for responsiveness effect/placebo
naive_difference[, placement := c(1:4)]

naive_difference[, Lower_Bound_50 := c(boots[, quantile(boot_effect_pi, .25)],
  boots[, quantile(boot_placebo_pi, .25)],
  boots[, quantile(boot_effect_pf, .25)],
  boots[, quantile(boot_placebo_pf, .25)])]
naive_difference[, Upper_Bound_50 := c(boots[, quantile(boot_effect_pi, .75)],
  boots[, quantile(boot_placebo_pi, .75)],
  boots[, quantile(boot_effect_pf, .75)],
  boots[, quantile(boot_placebo_pf, .75)])]

pdf(file="plots/senate-diff-in-diff-coeff-separate.pdf", ## RENAME
  width = 4, height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-2.5, 1.5), xlim=c(0.5, 4.5),
  cex.lab=1.1, xaxt="n", yaxt="n", xlab="", ylab="Effect")
axis(1, naive_difference$placement, cex.axis = .5,
  labels = c("Party Calls, Reelection", "Placebo",
    "Baseline, Reelection", "Placebo"))
axis(2, c(-2, -1.5, -1, -.5, 0, 0.5, 1), cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main="Party Call and Baseline Rate",
  cex.main=1, line=0.75, font.main=2)
points(naive_difference$placement, naive_difference$Estimate,
  pch=19, col="black", cex=.8)
segments(naive_difference$placement, naive_difference$Lower_Bound_50,
  naive_difference$placement,  naive_difference$Upper_Bound_50, lwd = 2.5)
segments(naive_difference$placement, naive_difference$Lower_Bound,
  naive_difference$placement,  naive_difference$Upper_Bound, lwd = 1)

dev.off()
