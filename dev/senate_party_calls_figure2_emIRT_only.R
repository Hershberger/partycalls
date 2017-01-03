# Produce replication of right column of Figure 2 from MV13

library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("")
whoheeds13 <- readstata13::read.dta13(
  "inst/extdata/who-heeds-replication-archive.dta")
setDT(whoheeds13)
whoheeds13 <- whoheeds13[, .(
  congress, icpsr, maj, retiree, bestgrosswart, dpres
)]
new_whoheeds13 <- merge(new_whoheeds13, whoheeds13, by = c("congress", "icpsr"))

f_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + dpres + south + votepct + female + afam + latino +
  seniority + freshman + retiree + bestgrosswart + leader +
  power + chair
fm_extremism <- function(i, j) {
  summary(lm(f_extremism,
    data = subset(new_whoheeds13, congress == i & maj == j)),
    vcov = vcovHC(type = "HC1"))
}
B <- SE <- data.frame(row.names = 93:109)
B$extremism_maj <- sapply(93:109, function(x)
  fm_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B$extremism_min <- sapply(93:109, function(x)
  fm_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE$extremism_maj <- sapply(93:109, function(x)
  fm_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE$extremism_min <- sapply(93:109, function(x)
  fm_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

pdf(file="plots/who-heeds-figure2-replication_hybrid.pdf", ## RENAME
  width = 4, height = 8, family = "Times")
layout(matrix(1:2, 2, 1, byrow = TRUE))
par(mar = c(2.5, 4, 2, 0.3) + 0.1, font.lab = 2)

x <- (93:109)#[-12]
x.ticks <- c(94, 99, 104, 109)
y.ticks <- c(-12, 0, 12, 24, 36)

b <- B$extremism_maj#[-12]
se <- SE$extremism_maj#[-12]
plot(0, 0, type='n', ylim=c(-6, 42), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Ideological Extremism", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b - qnorm(.750) * se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b - qnorm(.975) * se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism_min#[-12]
se <- SE$extremism_min#[-12]
plot(0, 0, type='n', ylim=c(-6, 42), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b - qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b - qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

dev.off()
