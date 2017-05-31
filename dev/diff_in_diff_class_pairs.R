library(partycalls)
library(xtable)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, y1 = pirate100, y2 = pfrate100)]

# subset to cases with two senators
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, y := y1 - y2]
DATA[, stabb_congress := paste0(stabb, congress)]

# get pairs based on distance to reelection
tr_dat <- DATA[tr == 1, .(congress, stabb, class, tr)]
ct_dat <- DATA[tr == 0, .(congress, stabb, class, tr)]
ct_dat <- merge(ct_dat, ct_dat[, .N, .(stabb, congress)])

dat_t <- merge(tr_dat, ct_dat[N == 1, ], by = c("congress", "stabb"))
dat_c <- ct_dat[N == 2, ]

pair_3_2 <- dat_t[class.x == 3 & class.y == 1 | class.x == 2 & class.y == 3 |
  class.x == 1 & class.y == 2, .(stabb, congress)]
pair_3_1 <- dat_t[class.x == 3 & class.y == 2 | class.x == 2 & class.y == 1 |
    class.x == 1 & class.y == 3, .(stabb, congress)]
pair_2_1 <- dat_c[, unique(stabb), by = congress]
setnames(pair_2_1, "V1", "stabb")

pair_3_1 <- merge(pair_3_1, DATA, by = c("stabb", "congress"), all.x = TRUE,
  all.y = FALSE)
pair_3_2 <- merge(pair_3_2, DATA, by = c("stabb", "congress"), all.x = TRUE,
  all.y = FALSE)
pair_2_1 <- merge(pair_2_1, DATA, by = c("stabb", "congress"), all.x = TRUE,
  all.y = FALSE)
pair_2_1[congress %in% seq(93, 111, 3) & class == 1 |
    congress %in% seq(94, 112, 3) & class == 2 |
    congress %in% seq(95, 110, 3) & class == 3, tr := 1]

# get effect estimates by pair type
diff_pi_3_2 <- pair_3_2[, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb, congress)][, mean(V1)]
diff_pf_3_2 <- pair_3_2[, sum(tr * y2) - sum((1 - tr) * y2),
  .(stabb, congress)][, mean(V1)]
diff_in_3_2 <- pair_3_2[, sum(tr * y) - sum((1 - tr) * y),
  .(stabb, congress)][, mean(V1)]

diff_pi_3_1 <- pair_3_1[, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb, congress)][, mean(V1)]
diff_pf_3_1 <- pair_3_1[, sum(tr * y2) - sum((1 - tr) * y2),
  .(stabb, congress)][, mean(V1)]
diff_in_3_1 <- pair_3_1[, sum(tr * y) - sum((1 - tr) * y),
  .(stabb, congress)][, mean(V1)]

diff_pi_2_1 <- pair_2_1[, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb, congress)][, mean(V1)]
diff_pf_2_1 <- pair_2_1[, sum(tr * y2) - sum((1 - tr) * y2),
  .(stabb, congress)][, mean(V1)]
diff_in_2_1 <- pair_2_1[, sum(tr * y) - sum((1 - tr) * y),
  .(stabb, congress)][, mean(V1)]

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
    .(stabb, congress)][, mean(V1)]
  boot_pf_diff <- boot_DATA[, sum(tr * y2) - sum((1 - tr) * y2),
    .(stabb, congress)][, mean(V1)]
  boot_in_diff <- boot_DATA[, sum(tr * y) - sum((1 - tr) * y),
    .(stabb, congress)][, mean(V1)]
  data.table(boot_pi_diff, boot_pf_diff, boot_in_diff)
}

boot_3_1 <- function(i) {
  boot_states <- sample(stabb_congress_3_1, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- pair_3_1[stabb_congress == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_pi_diff <- boot_DATA[, sum(tr * y1) - sum((1 - tr) * y1),
    .(stabb, congress)][, mean(V1)]
  boot_pf_diff <- boot_DATA[, sum(tr * y2) - sum((1 - tr) * y2),
    .(stabb, congress)][, mean(V1)]
  boot_in_diff <- boot_DATA[, sum(tr * y) - sum((1 - tr) * y),
    .(stabb, congress)][, mean(V1)]
  data.table(boot_pi_diff, boot_pf_diff, boot_in_diff)
}

boot_2_1 <- function(i) {
  boot_states <- sample(stabb_congress_2_1, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- pair_2_1[stabb_congress == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_pi_diff <- boot_DATA[, sum(tr * y1) - sum((1 - tr) * y1),
    .(stabb, congress)][, mean(V1)]
  boot_pf_diff <- boot_DATA[, sum(tr * y2) - sum((1 - tr) * y2),
    .(stabb, congress)][, mean(V1)]
  boot_in_diff <- boot_DATA[, sum(tr * y) - sum((1 - tr) * y),
    .(stabb, congress)][, mean(V1)]
  data.table(boot_pi_diff, boot_pf_diff, boot_in_diff)
}

set.seed(138720496)
boots_3_2 <- rbindlist(lapply(1:1000, boot_3_2))
set.seed(1192696137)
boots_3_1 <- rbindlist(lapply(1:1000, boot_3_1))
set.seed(1781071333)
boots_2_1 <- rbindlist(lapply(1:1000, boot_2_1))

diff_3_2 = data.table(
  Group = "Term Parts 3 and 2",
  Test = c("Party Call Diff", "Party Free Diff", "Diff-in-Diff"),
  Estimate = c(diff_pi_3_2, diff_pf_3_2, diff_in_3_2),
  Lower_CI = c(boots_3_2[, quantile(boot_pi_diff, .05)],
    boots_3_2[, quantile(boot_pf_diff, .05)],
    boots_3_2[, quantile(boot_in_diff, .05)]),
  Upper_CI = c(boots_3_2[, quantile(boot_pi_diff, .95)],
    boots_3_2[, quantile(boot_pf_diff, .95)],
    boots_3_2[, quantile(boot_in_diff, .95)])
)

diff_3_1 = data.table(
  Group = "Term Parts 3 and 1",
  Test = c("Party Call Diff", "Party Free Diff", "Diff-in-Diff"),
  Estimate = c(diff_pi_3_1, diff_pf_3_1, diff_in_3_1),
  Lower_CI = c(boots_3_1[, quantile(boot_pi_diff, .05)],
    boots_3_1[, quantile(boot_pf_diff, .05)],
    boots_3_1[, quantile(boot_in_diff, .05)]),
  Upper_CI = c(boots_3_1[, quantile(boot_pi_diff, .95)],
    boots_3_1[, quantile(boot_pf_diff, .95)],
    boots_3_1[, quantile(boot_in_diff, .95)])
)

diff_2_1 = data.table(
  Group = "Term Parts 2 and 1",
  Test = c("Party Call Diff", "Party Free Diff", "Diff-in-Diff"),
  Estimate = c(diff_pi_2_1, diff_pf_2_1, diff_in_2_1),
  Lower_CI = c(boots_2_1[, quantile(boot_pi_diff, .05)],
    boots_2_1[, quantile(boot_pf_diff, .05)],
    boots_2_1[, quantile(boot_in_diff, .05)]),
  Upper_CI = c(boots_2_1[, quantile(boot_pi_diff, .95)],
    boots_2_1[, quantile(boot_pf_diff, .95)],
    boots_2_1[, quantile(boot_in_diff, .95)])
)

difference_3_2_tex <- xtable(diff_3_2, auto = TRUE,
  caption = "Differences Between Same-State Senators in 3rd and 2nd Congress in Term",
  digits = c(3, 3, 3, 3, 3, 3))
difference_3_1_tex <- xtable(diff_3_1, auto = TRUE,
  caption = "Differences Between Same-State Senators in  3rd and 1st Congress in Term",
  digits = c(3, 3, 3, 3, 3, 3))
difference_2_1_tex <- xtable(diff_2_1, auto = TRUE,
  caption = "Differences Between Same-State Senators in  2nd and 1st Congress in Term",
  digits = c(3, 3, 3, 3, 3, 3))

print(difference_3_2_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")
print(difference_3_1_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")
print(difference_2_1_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")
