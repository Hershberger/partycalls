library(partycalls)
library(sandwich)
library(lmtest)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, y1 = pirate100, y2 = pfrate100, retiree)]

# subset to cases with two senators
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, stabb_congress := paste0(stabb, congress)]
DATA <- DATA[, mean_retiree := mean(retiree), .(stabb_congress)]
DATA <- DATA[mean_retiree == 0, ]

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
pair_3_1[, pair31 := 1]
pair_3_1[, pair32 := 0]
pair_3_1[, pair21 := 0]

pair_3_2 <- merge(pair_3_2, DATA, by = c("stabb", "congress"), all.x = TRUE,
  all.y = FALSE)
pair_3_2[, pair31 := 0]
pair_3_2[, pair32 := 1]
pair_3_2[, pair21 := 0]

pair_2_1 <- merge(pair_2_1, DATA, by = c("stabb", "congress"), all.x = TRUE,
  all.y = FALSE)
pair_2_1[congress %in% seq(93, 111, 3) & class == 1 |
    congress %in% seq(94, 112, 3) & class == 2 |
    congress %in% seq(95, 110, 3) & class == 3, tr := 1]
pair_2_1[, pair31 := 0]
pair_2_1[, pair32 := 0]
pair_2_1[, pair21 := 1]

DATA <- rbind(pair_3_1, pair_3_2, pair_2_1)

mod_pi <- lm(y1 ~ tr * pair31 + tr * pair32, data = DATA)
mod_pi$newse <- vcovHC(mod_pi)
mod_pi$ci <- coefci(x = mod_pi, vcov = mod_pi$newse, level = .9)

mod_pf <- lm(y2 ~ tr * pair31 + tr * pair32, data = DATA)
mod_pf$newse <- vcovHC(mod_pf)
mod_pf$ci <- coefci(x = mod_pf, vcov = mod_pf$newse, level = .9)

ests_pi <- data.table(
  est = c(
    mod_pi$coefficients["tr:pair32"], mod_pi$coefficients["pair32"],
    mod_pi$coefficients["tr:pair31"], mod_pi$coefficients["pair31"],
    mod_pi$coefficients["tr"]),
  lower = c(mod_pi$ci[[6]], mod_pi$ci[[4]], mod_pi$ci[[5]], mod_pi$ci[[3]],
    mod_pi$ci[[2]]),
  upper = c(mod_pi$ci[[12]], mod_pi$ci[[10]], mod_pi$ci[[11]], mod_pi$ci[[9]],
    mod_pi$ci[[8]]),
  position = c(1:5)
)

ests_pf <- data.table(
  est = c(mod_pf$coefficients["tr:pair32"], mod_pf$coefficients["pair32"],
    mod_pf$coefficients["tr:pair31"], mod_pf$coefficients["pair31"],
    mod_pf$coefficients["tr"]),
  lower = c(mod_pf$ci[[6]], mod_pf$ci[[4]], mod_pf$ci[[5]], mod_pf$ci[[3]],
    mod_pf$ci[[2]]),
  upper = c(mod_pf$ci[[12]], mod_pf$ci[[10]], mod_pf$ci[[11]], mod_pf$ci[[9]],
    mod_pf$ci[[8]]),
  position = c(1:5)
)

  # coef plots
  pdf(file = "plots/senate_reelection_interaction_pi_robust_se.pdf", width = 6,
    height = 4, family = "Times")

  plot(0, 0, type='n', ylim=c(-2.75, 3), xlim=c(0.5, 5.5),
    cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Estimate")
  axis(1, ests_pi$position, cex.axis = .7,
    labels = c("Cong. 3-2\nTreated", "Control",
      "Cong. 3-1\nTreated", "Control", "Cong.\n2-1 Treated"))
  axis(2, c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5),
    cex.axis = 1.1, labels = TRUE)
  abline(h=0, col="gray55", xpd=FALSE)
  title(main = "Estimated Response Difference by Congress in Term")
  points(ests_pi$position, ests_pi$est,
    pch=19, col="black", cex=.8)
  segments(ests_pi$position, ests_pi$lower,
    ests_pi$position,  ests_pi$upper, lwd = 1)

  dev.off()


  pdf(file = "plots/senate_reelection_interaction_pf_robust_se.pdf", width = 6,
    height = 4, family = "Times")

  plot(0, 0, type='n', ylim=c(-2.75, 3), xlim=c(0.5, 5.5),
    cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Estimate")
  axis(1, ests_pf$position, cex.axis = .7,
    labels = c("Cong. 3-2\nTreated", "Control",
      "Cong. 3-1\nTreated", "Control", "Cong. 2-1\nTreated"))
  axis(2, c(-2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5),
    cex.axis = 1.1, labels = TRUE)
  abline(h=0, col="gray55", xpd=FALSE)
  title(main = "Estimated Response Difference by Congress in Term")
  points(ests_pf$position, ests_pf$est,
    pch=19, col="black", cex=.8)
  segments(ests_pf$position, ests_pf$lower,
    ests_pf$position,  ests_pf$upper, lwd = 1)

  dev.off()
