# Produce replication of right column of Figure 2 from MV13

library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[, dem := 0]
senate_data[caucus == "Democrat", dem := 1]

f_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + up_for_reelection + vote_share +
  female + afam + latino + maj + south +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair
fm_extremism <- function(i, j) {
  summary(lm(f_extremism,
    data = subset(senate_data, congress == i & dem == j)),
    vcov = vcovHC(type = "HC1"))
}

B <- SE <- data.frame(row.names = 93:112)
B$extremism_south <- sapply(93:112, function(x)
  fm_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B$extremism_other <- sapply(93:112, function(x)
  fm_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE$extremism_south <- sapply(93:112, function(x)
  fm_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE$extremism_other <- sapply(93:112, function(x)
  fm_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

pdf(file="plots/senate-figure2-party.pdf", ## RENAME
  width = 4, height = 8, family = "Times")
layout(matrix(1:2, 2, 1, byrow = TRUE))
par(mar = c(2.5, 4, 2, 0.3) + 0.1, font.lab = 2)

x <- (93:112)
x.ticks <- c(95, 100, 105, 110)
y.ticks <- c(- 24, -12, -6, 0, 12, 24, 36)

b <- B$extremism_south
se <- SE$extremism_south
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Democrats")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Ideological Extremism", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b - qnorm(.750) * se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b - qnorm(.975) * se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism_other
se <- SE$extremism_other
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Republicans")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b - qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b - qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

dev.off()
