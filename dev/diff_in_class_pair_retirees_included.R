library(partycalls)
library(xtable)
library(data.table)
set.seed(1781071333)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  y1 = pirate100, y2 = pfrate100)]

# subset to cases with two senators
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, stabb_congress := paste0(stabb, congress)]
DATA[, tr := 0]

# make place in term variable
DATA[, in_term := 1]

DATA[class == 3 & congress %in% seq(93, 111, 3), in_term := 3]
DATA[class == 3 & congress %in% seq(95, 110, 3), in_term := 2]

DATA[class == 2 & congress %in% seq(95, 110, 3), in_term := 3]
DATA[class == 2 & congress %in% seq(94, 112, 3), in_term := 2]

DATA[class == 1 & congress %in% seq(94, 112, 3), in_term := 3]
DATA[class == 1 & congress %in% seq(93, 111, 3), in_term := 2]

# make pair datasets
pair_3 <- DATA[in_term == 3, ]
pair_2 <- DATA[in_term == 2, ]
pair_1 <- DATA[in_term == 1, ]

pair_3_2 <- merge(pair_3, pair_2, by = "stabb_congress", all.x = FALSE, all.y = FALSE)
pair_3_1 <- merge(pair_3, pair_1, by = "stabb_congress", all.x = FALSE, all.y = FALSE)
pair_2_1 <- merge(pair_2, pair_1, by = "stabb_congress", all.x = FALSE, all.y = FALSE)

pair_3_2 <- DATA[stabb_congress %in% pair_3_2$stabb_congress, ]
pair_3_1 <- DATA[stabb_congress %in% pair_3_1$stabb_congress, ]
pair_2_1 <- DATA[stabb_congress %in% pair_2_1$stabb_congress, ]

pair_3_2[in_term == 3, tr := 1]
pair_3_1[in_term == 3, tr := 1]
pair_2_1[in_term == 2, tr := 1]

# get effect estimates by pair type
diff_pi_3_2 <- pair_3_2[, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb_congress)][, mean(V1)]
diff_pf_3_2 <- pair_3_2[, sum(tr * y2) - sum((1 - tr) * y2),
  .(stabb_congress)][, mean(V1)]

diff_pi_3_1 <- pair_3_1[, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb_congress)][, mean(V1)]
diff_pf_3_1 <- pair_3_1[, sum(tr * y2) - sum((1 - tr) * y2),
  .(stabb_congress)][, mean(V1)]

diff_pi_2_1 <- pair_2_1[, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb_congress)][, mean(V1)]
diff_pf_2_1 <- pair_2_1[, sum(tr * y2) - sum((1 - tr) * y2),
  .(stabb_congress)][, mean(V1)]

# get bootstrap state/congress pairs
stabb_congress_3_2 <- unique(pair_3_2$stabb_congress)
stabb_congress_3_1 <- unique(pair_3_1$stabb_congress)
stabb_congress_2_1 <- unique(pair_2_1$stabb_congress)

boot_3_2 <- function(i) {
  boot_states <- sample(stabb_congress_3_2, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- pair_3_2[stabb_congress == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_pi_diff <- boot_DATA[, sum(tr * y1) - sum((1 - tr) * y1),
    .(stabb_congress)][, mean(V1)]
  boot_pf_diff <- boot_DATA[, sum(tr * y2) - sum((1 - tr) * y2),
    .(stabb_congress)][, mean(V1)]
  data.table(boot_pi_diff, boot_pf_diff)
}

boot_3_1 <- function(i) {
  boot_states <- sample(stabb_congress_3_1, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- pair_3_1[stabb_congress == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_pi_diff <- boot_DATA[, sum(tr * y1) - sum((1 - tr) * y1),
    .(stabb_congress)][, mean(V1)]
  boot_pf_diff <- boot_DATA[, sum(tr * y2) - sum((1 - tr) * y2),
    .(stabb_congress)][, mean(V1)]
  data.table(boot_pi_diff, boot_pf_diff)
}

boot_2_1 <- function(i) {
  boot_states <- sample(stabb_congress_2_1, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- pair_2_1[stabb_congress == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_pi_diff <- boot_DATA[, sum(tr * y1) - sum((1 - tr) * y1),
    .(stabb_congress)][, mean(V1)]
  boot_pf_diff <- boot_DATA[, sum(tr * y2) - sum((1 - tr) * y2),
    .(stabb_congress)][, mean(V1)]
  data.table(boot_pi_diff, boot_pf_diff)
}

boots_3_2 <- rbindlist(lapply(1:1000, boot_3_2))
boots_3_1 <- rbindlist(lapply(1:1000, boot_3_1))
boots_2_1 <- rbindlist(lapply(1:1000, boot_2_1))

diff_3_2 = data.table(
  Group = "Term Parts 3 and 2",
  Test = c("Party Call Diff", "Party Free Diff"),
  Estimate = c(diff_pi_3_2, diff_pf_3_2),
  Lower_CI = c(boots_3_2[, quantile(boot_pi_diff, .05)],
    boots_3_2[, quantile(boot_pf_diff, .05)]),
  Upper_CI = c(boots_3_2[, quantile(boot_pi_diff, .95)],
    boots_3_2[, quantile(boot_pf_diff, .95)])
)

diff_3_1 = data.table(
  Group = "Term Parts 3 and 1",
  Test = c("Party Call Diff", "Party Free Diff"),
  Estimate = c(diff_pi_3_1, diff_pf_3_1),
  Lower_CI = c(boots_3_1[, quantile(boot_pi_diff, .05)],
    boots_3_1[, quantile(boot_pf_diff, .05)]),
  Upper_CI = c(boots_3_1[, quantile(boot_pi_diff, .95)],
    boots_3_1[, quantile(boot_pf_diff, .95)])
)

diff_2_1 = data.table(
  Group = "Term Parts 2 and 1",
  Test = c("Party Call Diff", "Party Free Diff"),
  Estimate = c(diff_pi_2_1, diff_pf_2_1),
  Lower_CI = c(boots_2_1[, quantile(boot_pi_diff, .05)],
    boots_2_1[, quantile(boot_pf_diff, .05)]),
  Upper_CI = c(boots_2_1[, quantile(boot_pi_diff, .95)],
    boots_2_1[, quantile(boot_pf_diff, .95)])
)

diff <- rbind(diff_3_2, diff_3_1, diff_2_1)

# make plot
diff[, position := c(1, 2, 3.5, 4.5, 6, 7)]
diff

# coef plot
pdf(file = "plots/diff_in_class_pair_retirees_included.pdf", width = 6,
  height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-4.75, 1.6), xlim=c(0.5, 7.5),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Estimate")
axis(1, diff$position, cex.axis = .7,
  labels = c("Cong. 3-2 \n Party Calls", "Party Free",
    "Cong. 3-1 \n Party Calls", "Party Free",
    "Cong. 2-1 \n Party Calls", "Party Free"))
axis(2, c(-4.5, -4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5),
  cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main = "Paired Response Difference by Congress in Term")
points(diff$position, diff$Estimate,
  pch=19, col="black", cex=.8)
segments(diff$position, diff$Lower_CI,
  diff$position,  diff$Upper_CI, lwd = 1)

dev.off()
