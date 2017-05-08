library(partycalls)
library(xtable)
set.seed(98037298)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  votes, tr = up_for_reelection, y = pirate100 - pfrate100)]
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
effect <- DATA[mean_tr == .5,
  sum(tr * y) - sum((1 - tr) * y), .(stabb, congress)][,
    mean(V1)]
placebo <- DATA[mean_tr == 0,
  sum((votes < mean(votes)) * y) - sum((votes > mean(votes)) * y),
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
  boot_placebo <- boot_DATA[mean_tr == 0,
    sum((votes < mean(votes)) * y) - sum((votes > mean(votes)) * y),
    .(boot_id, congress)][,
      mean(V1)]
  data.table(boot_effect, boot_placebo)
}

boots <- rbindlist(lapply(1:1000, boot))
naive_difference <- data.table(test = c("Effect", "Placebo"),
  DV = "pirate100 - pfrate100",
  Estimate = c(effect, placebo),
  Lower_Bound = c(boots[, quantile(boot_effect, .025)],
    boots[, quantile(boot_placebo, .025)]),
  Upper_Bound = c(boots[, quantile(boot_effect, .975)],
    boots[, quantile(boot_placebo, .975)])
)

DATA[, stabb_congress := paste(stabb, congress)]
# summary(lfe::felm(y ~ tr | stabb_congress | 0 | stabb_congress,
#   DATA[mean_tr == .5]))
# summary(lfe::felm(y ~ tr | stabb_congress | 0 | stabb_congress,
#   DATA[mean_tr == 0]))
#
# summary(lfe::felm(y ~ tr + factor(seat_pair_type) |
#     stabb_congress | 0 | stabb_congress,
#   DATA))


DATA_1 <- DATA[tr == 1]
DATA_0 <- DATA[tr == 0]
DATA_pairs <- merge(DATA_1, DATA_0, by = c("stabb", "congress", "seat_pair_type"))


seat_type_effect <- DATA[mean_tr == .5, sum(tr * y) - sum((1 - tr) * y),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_effect, "V1", "effect")
seat_type_effect

seat_type_placebo <- DATA[, sum((votes < mean(votes)) * y) -
    sum(((votes) > mean(votes)) * y),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_placebo, "V1", "placebo")
seat_type_placebo

seat_type_difference <- data.table(
  Test = c("2 Maj Dems Effect", "2 Maj Dems Placebo",
    "2 Min Dems Effect", "2 Min Dems Placebo",
    "2 Maj Reps Effect", "2 Maj Reps Placebo",
    "2 Min Reps Effect", "2 Min Reps Placebo",
    "Split, Maj Dem, Dem Effect", "Split, Maj Dem, Dem Placebo",
    "Split, Maj Dem, Rep Effect", "Split, Maj Dem, Rep Placebo",
    "Split, Maj Rep, Dem Effect", "Split, Maj Rep, Dem Placebo",
    "Split, Maj Rep, Rep Effect", "Split, Maj Rep, Rep Placebo"),
  DV = "pirate100 - pfrate100",
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

# make coeff plot from effects
naive_difference[, position := 0]
naive_difference[test == "Placebo", position := 1]
naive_difference[, Lower_Bound_50 := c(boots[, quantile(boot_effect, .25)],
  boots[, quantile(boot_placebo, .25)])]
naive_difference[, Upper_Bound_50 := c(boots[, quantile(boot_effect, .75)],
  boots[, quantile(boot_placebo, .75)])]

pdf(file="plots/senate_diff_in_diff_vote_placebo.pdf", ## RENAME
  width = 4, height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-1.8, 1.8), xlim=c(-0.5, 1.5),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Effect")
axis(1, naive_difference$position, cex.axis = 1.1,
  labels = c("Reelection Treatment", "Placebo"))
axis(2, c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5), cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main="Party Call Rate Difference from Baseline",
  cex.main=1, line=0.75, font.main=2)
points(naive_difference$position, naive_difference$Estimate,
  pch=19, col="black", cex=.8)
segments(naive_difference$position, naive_difference$Lower_Bound_50,
  naive_difference$position,  naive_difference$Upper_Bound_50, lwd = 2.5)
segments(naive_difference$position, naive_difference$Lower_Bound,
  naive_difference$position,  naive_difference$Upper_Bound, lwd = 1)

dev.off()
