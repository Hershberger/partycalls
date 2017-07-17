library(partycalls)
library(xtable)
set.seed(1339367524)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  votes, tr = up_for_reelection, y1 = pirate100, y2 = pfrate100)]
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

# Estimate Effects
diff_pi <- DATA[mean_tr == .5,
  sum(tr * y1) - sum((1 - tr) * y1), .(stabb, congress)][,
    mean(V1)]
diff_pf <- DATA[mean_tr == .5,
  sum(tr * y2) - sum((1 - tr) * y2), .(stabb, congress)][,
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
  boot_diff_pi <- boot_DATA[mean_tr == .5,
    sum(tr * y1) - sum((1 - tr) * y1), .(boot_id, congress)][,
      mean(V1)]
  boot_diff_pf <- boot_DATA[mean_tr == .5,
    sum(tr * y2) - sum((1 - tr) * y2), .(boot_id, congress)][,
      mean(V1)]
  data.table(boot_diff_pi, boot_diff_pf)
}

boots <- rbindlist(lapply(1:1000, boot))
differences <- data.table(test = c("Party Call Difference",
  "Party Free Difference"),
  Estimate = c(diff_pi, diff_pf),
  Lower_Bound = c(boots[, quantile(boot_diff_pi, .025)],
    boots[, quantile(boot_diff_pf, .025)]),
  Upper_Bound = c(boots[, quantile(boot_diff_pi, .975)],
    boots[, quantile(boot_diff_pf, .975)]),
  Lower_50 = c(boots[, quantile(boot_diff_pi, 0.25)],
    boots[, quantile(boot_diff_pf, 0.25)]),
  Upper_50 = c(boots[, quantile(boot_diff_pi, 0.75)],
    boots[, quantile(boot_diff_pf, 0.75)])
)

# difference_tex <- xtable(differences, auto = TRUE,
#   caption = "Reelection and Response to Party Calls, Same-State Senator Differences",
#   digits = c(3, 3, 3, 3, 3))
# print(difference_tex, include.rownames = FALSE,
#   table.placement = "H", caption.placement = "top")

# make coeff plot from effects
differences[, position := 0]
differences[test == "Party Free Difference", position := 1]


pdf(file="plots/senate_difference_estimates.pdf", ## RENAME
  width = 5, height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-2.2, .3), xlim=c(-0.5, 1.5),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Effect")
axis(1, differences$position, cex.axis =.8,
  labels = c("Party Call Rate", "Baseline Rate"))
axis(2, c(-2, -1.5, -1, -0.5, 0), cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main="Same-State Pair Differences, Reelection Treatment",
  cex.main=.8, line=0.75, font.main=2)
points(differences$position, differences$Estimate,
  pch=19, col="black", cex=.8)
segments(differences$position, differences$Lower_Bound,
  differences$position,  differences$Upper_Bound, lwd = 1)
segments(differences$position,
  differences$Lower_50, differences$position, differences$Upper_50, lwd=2)

dev.off()
