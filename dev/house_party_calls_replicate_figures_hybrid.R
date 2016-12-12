library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

# load data for analysis
load("data/house_party_calls_replication_hybrid.RData")
whoheeds13 <- readstata13::read.dta13(
  "inst/extdata/who-heeds-replication-archive.dta")
setDT(new_whoheeds13)
setDT(whoheeds13)
new_whoheeds13 <- merge(new_whoheeds13, whoheeds13, by = c("congress", "icpsr"))
setDT(new_whoheeds13)

# regression formulas and functions
f.meddist <- new_pirate100 ~ new_distance_from_floor_median +
  new_pfrate100 + dpres_pct + south.x + votepct.x + female.x + afam.x + latino.x +
  seniority.x + freshman.x + retiree + bestgrosswart + leader.x +
  power.x + chair.x
f.extremism <- new_pirate100 ~ new_ideological_extremism +
  new_pfrate100 + dpres_pct + south.x + votepct.x + female.x + afam.x + latino.x +
  seniority.x + freshman.x + retiree + bestgrosswart + leader.x +
  power.x + chair.x

# To see a regression from a particular congress, use these functions:
fm.meddist <- function(i, j) {
  summary(lm(f.meddist,
    data=subset(new_whoheeds13, congress==i & maj==j)),
    vcov=vcovHC(type="HC1"))
}
fm.extremism <- function(i, j) {
  summary(lm(f.extremism,
    data=subset(new_whoheeds13, congress==i & maj==j)),
    vcov=vcovHC(type="HC1"))
}

# Uncomment to use
# fm.meddist(93, 1) # <- produces regression results for Distance from the
#    Median Model for majority party in 93rd Congress;
#    change 1 to 0 for minority party


# Produce results for Figure 2
B <- SE <- data.frame(row.names=93:109)
B$meddist.maj <- do.call(c, lapply(93:109, function(x)
  fm.meddist(x, 1)$coef[2, 1]))
B$meddist.min <- do.call(c, lapply(93:109, function(x)
  fm.meddist(x, 0)$coef[2, 1]))
B$extremism.maj <- do.call(c, lapply(93:109, function(x)
  fm.extremism(x, 1)$coef[2, 1]))
B$extremism.min <- do.call(c, lapply(93:109, function(x)
  fm.extremism(x, 0)$coef[2, 1]))
SE$meddist.maj <- do.call(c, lapply(93:109, function(x)
  fm.meddist(x, 1)$coef[2, 2]))
SE$meddist.min <- do.call(c, lapply(93:109, function(x)
  fm.meddist(x, 0)$coef[2, 2]))
SE$extremism.maj <- do.call(c, lapply(93:109, function(x)
  fm.extremism(x, 1)$coef[2, 2]))
SE$extremism.min <- do.call(c, lapply(93:109, function(x)
  fm.extremism(x, 0)$coef[2, 2]))

pdf(file="plots/who-heeds-figure2-replication_hybrid.pdf", ## RENAME
  width=4, height = 8, family="Times")
layout(matrix(1:2, 2, 1, byrow=TRUE))
par(mar=c(2.5, 4, 2, 0.3) + 0.1, font.lab=2)

x <- (93:109)[-12]
x.ticks <- c(94, 99, 104, 109)
y.ticks <- c(-12, 0, 12, 24)

# b <- B$meddist.maj[-12]
# se <- SE$meddist.maj[-12]
# plot(0, 0, type='n', ylim=c(-6, 36), xlim=c(93, 109),
#   cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Majority Party")
# axis(1, x.ticks, cex.axis=1.1, labels=FALSE, xpd=TRUE)
# axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
# abline(h=0, col="gray", xpd=FALSE)
# title(main="Distance from Floor Median", cex.main=1.15, line=0.75, font.main=2)
# points(x, b, pch=19, col="black", cex=.8)
# segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
# segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism.maj[-12]
se <- SE$extremism.maj[-12]
plot(0, 0, type='n', ylim=c(-6, 36), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Ideological Extremism", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

# b <- B$meddist.min[-12]
# se <- SE$meddist.min[-12]
# plot(0, 0, type='n', ylim=c(-6, 36), xlim=c(93, 109),
#   cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Minority Party")
# axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
# axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
# abline(h=0, col="gray", xpd=FALSE)
# title(main="", cex.main=1.15, line=0.75, font.main=2)
# points(x, b, pch=19, col="black", cex=.8)
# segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
# segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism.min[-12]
se <- SE$extremism.min[-12]
plot(0, 0, type='n', ylim=c(-6, 36), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

# NAME FIGURE WITH CLASSIFICATION METHOD IN TEX CAPTION

dev.off()

