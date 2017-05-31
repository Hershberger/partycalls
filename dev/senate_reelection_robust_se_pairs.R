library(partycalls)
library(sandwich)
library(lmtest)

load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, pirate100, pfrate100, retiree)]

# subset to cases with two senators
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, stabb_congress := paste0(stabb, congress)]
DATA[, mean_tr := mean(tr), .(stabb, congress)]

DATA <- DATA[retiree == 0, ]

# get pairs based on distance to reelection
dat_t <- merge(DATA[tr == 1, .(stabb_congress, class)],
  DATA[tr == 0 & mean_tr == 0.5, .(stabb_congress, class)],
  by = c("stabb_congress"))

pair_3_2 <- dat_t[class.x == 3 & class.y == 1 |
  class.x == 2 & class.y == 3 | class.x == 1 & class.y == 2, .(stabb_congress)]
pair_3_1 <- dat_t[class.x == 3 & class.y == 2 |
  class.x == 2 & class.y == 1 | class.x == 1 & class.y == 3, .(stabb_congress)]
pair_2_1 <- DATA[mean_tr == 0, unique(stabb_congress)]

DATA_3_2 <- DATA[stabb_congress %in% pair_3_2$stabb_congress, ]
DATA_3_1 <- DATA[stabb_congress %in% pair_3_1$stabb_congress, ]
DATA_2_1 <- DATA[stabb_congress %in% pair_2_1, ]

DATA_2_1[congress %in% seq(93, 111, 3) & class == 1 |
    congress %in% seq(94, 112, 3) & class == 2 |
    congress %in% seq(95, 110, 3) & class == 3, tr := 1]

# tests
mod_pi32 <- lm(pirate100 ~ tr, DATA_3_2)
mod_pi32$newse <- vcovHC(mod_pi32)
mod_pi32$ci <- coefci(x = mod_pi32, vcov = mod_pi32$newse, level = .9)
pi32 <- data.table(est = mod_pi32$coefficients["tr"], lower = mod_pi32$ci[[2]],
  upper = mod_pi32$ci[[4]], position = 1)

mod_pf32 <- lm(pfrate100 ~ tr, DATA_3_2)
mod_pf32$newse <- vcovHC(mod_pf32)
mod_pf32$ci <- coefci(x = mod_pf32, vcov = mod_pf32$newse, level = .9)
pf32 <- data.table(est = mod_pf32$coefficients["tr"], lower = mod_pf32$ci[[2]],
  upper = mod_pf32$ci[[4]], position = 2)

mod_pi31 <- lm(pirate100 ~ tr, DATA_3_1)
mod_pi31$newse <- vcovHC(mod_pi31)
mod_pi31$ci <- coefci(x = mod_pi31, vcov = mod_pi31$newse, level = .9)
pi31 <- data.table(est = mod_pi31$coefficients["tr"], lower = mod_pi31$ci[[2]],
  upper = mod_pi31$ci[[4]], position = 3.5)

mod_pf31 <- lm(pfrate100 ~ tr, DATA_3_1)
mod_pf31$newse <- vcovHC(mod_pf31)
mod_pf31$ci <- coefci(x = mod_pf31, vcov = mod_pf31$newse, level = .9)
pf31 <- data.table(est = mod_pf31$coefficients["tr"], lower = mod_pf31$ci[[2]],
  upper = mod_pf31$ci[[4]], position = 4.5)

mod_pi21 <- lm(pirate100 ~ tr, DATA_2_1)
mod_pi21$newse <- vcovHC(mod_pi21)
mod_pi21$ci <- coefci(x = mod_pi21, vcov = mod_pi21$newse, level = .9)
pi21 <- data.table(est = mod_pi21$coefficients["tr"], lower = mod_pi21$ci[[2]],
  upper = mod_pi21$ci[[4]], position = 6)

mod_pf21 <- lm(pfrate100 ~ tr, DATA_2_1)
mod_pf21$newse <- vcovHC(mod_pf21)
mod_pf21$ci <- coefci(x = mod_pf21, vcov = mod_pf21$newse, level = .9)
pf21 <- data.table(est = mod_pf21$coefficients["tr"], lower = mod_pf21$ci[[2]],
  upper = mod_pf21$ci[[4]], position = 7)

ests <- rbind(pi32, pf32, pi31, pf31, pi21, pf21)

# coef plot
pdf(file = "plots/senate_reelection_robust_se_pairs.pdf", width = 6,
  height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-3.25, 1.5), xlim=c(0.5, 7.5),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Estimate")
axis(1, ests$position, cex.axis = .7,
  labels = c("Cong. 3-2 \n Party Calls", "Party Free",
    "Cong. 3-1 \n Party Calls", "Party Free",
    "Cong. 2-1 \n Party Calls", "Party Free"))
axis(2, c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1), cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main = "Estimated Response Difference for Reelection Proximity")
points(ests$position, ests$est,
  pch=19, col="black", cex=.8)
segments(ests$position, ests$lower,
  ests$position,  ests$upper, lwd = 1)

dev.off()
