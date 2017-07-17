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

DATA[, term_place := 1]
DATA[tr == 1, term_place := 3]
DATA[class == 1 & congress %in% seq(93, 111, 3) |
  class == 2 & congress %in% seq(94, 112, 3) |
  class == 3 & congress %in% seq(95, 110, 3), term_place := 2]

DATA_3_2 <- DATA[term_place != 1, ]
DATA_3_1 <- DATA[term_place != 2, ]
DATA_2_1 <- DATA[term_place != 3, ]
DATA_2_1[term_place == 2, tr := 1]

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
ests

z_test2 <- function(a, b, var_a, var_b){
  n_a = length(a)
  n_b = length(b)
  zeta = (mean(a) - mean(b)) / (sqrt(var_a/n_a + var_b/n_b))
  return(zeta)
}

mod_pi32_robust <- coeftest(mod_pi32, vcov = sandwich)
mod_pi31_robust <- coeftest(mod_pi31, vcov = sandwich)
mod_pi21_robust <- coeftest(mod_pi21, vcov = sandwich)

z_test2(mod_pi32_robust[[2]], mod_pi21_robust[[2]],
  (mod_pi32_robust[[4]])^2, (mod_pi21_robust[[4]])^2)
z_test2(mod_pi31_robust[[2]], mod_pi21_robust[[2]],
  (mod_pi31_robust[[4]])^2, (mod_pi21_robust[[4]])^2)

mod_pf32_robust <- coeftest(mod_pf32, vcov = sandwich)
mod_pf31_robust <- coeftest(mod_pf31, vcov = sandwich)
mod_pf21_robust <- coeftest(mod_pf21, vcov = sandwich)

z_test2(mod_pf32_robust[[2]], mod_pf21_robust[[2]],
  (mod_pf32_robust[[4]])^2, (mod_pf21_robust[[4]])^2)
z_test2(mod_pf31_robust[[2]], mod_pf21_robust[[2]],
  (mod_pf31_robust[[4]])^2, (mod_pf21_robust[[4]])^2)

# coef plot
pdf(file = "plots/senate_reelection_robust_se.pdf", width = 6,
  height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-3.25, 1.5), xlim=c(0.5, 7.5),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Estimate")
axis(1, ests$position, cex.axis = .7,
  labels = c("Cong. 3-2 \n Party Calls", "Party Free",
    "Cong. 3-1 \n Party Calls", "Party Free",
    "Cong. 2-1 \n Party Calls", "Party Free"))
axis(2, c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1), cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main = "Estimated Response Difference by Congress in Term")
points(ests$position, ests$est,
  pch=19, col="black", cex=.8)
segments(ests$position, ests$lower,
  ests$position,  ests$upper, lwd = 1)

dev.off()
