library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]

load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
new_whoheeds13[, vote_share := vote_share - mean(vote_share, na.rm = TRUE)]
new_whoheeds13[is.na(vote_share) == TRUE, vote_share := 0]

sen_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share +
  female + afam + latino + up_for_reelection +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair
s_extremism <- function(i, j) {
  summary(lm(sen_extremism,
    data = subset(senate_data, congress == i & maj == j)),
    vcov = vcovHC(type = "HC1"))
}

B_s <- SE_s <- data.frame(row.names = 93:112)
B_s$extremism_maj <- sapply(93:112, function(x)
  s_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B_s$extremism_min <- sapply(93:112, function(x)
  s_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE_s$extremism_maj <- sapply(93:112, function(x)
  s_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE_s$extremism_min <- sapply(93:112, function(x)
  s_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

hou_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share + female + afam + latino +
  seniority + freshman + bestgrosswart + leader +
  power + chair
h_extremism <- function(i, j) {
  summary(lm(hou_extremism,
    data = subset(new_whoheeds13, congress == i & majority == j)),
    vcov = vcovHC(type = "HC1"))
}

B_h <- SE_h <- data.frame(row.names = 93:112)
B_h$extremism_maj <- sapply(93:112, function(x)
  h_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B_h$extremism_min <- sapply(93:112, function(x)
  h_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE_h$extremism_maj <- sapply(93:112, function(x)
  h_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE_h$extremism_min <- sapply(93:112, function(x)
  h_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

pdf(file="plots/both-chambers-figure2.pdf", ## RENAME
  width = 8, height = 8, family = "Times")
layout(matrix(1:4, 2, 2, byrow = TRUE))
par(mar = c(2.5, 4, 2, 0.3) + 0.1, font.lab = 2)

x <- (93:112)
x.ticks <- c(95, 100, 105, 110)
y.ticks <- c(- 24, -12, -6, 0, 12)

b_h <- B_h$extremism_maj#[-12]
se_h <- SE_h$extremism_maj#[-12]
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Majority")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="House", cex.main=1.15, line=0.75, font.main=2)
points(x, b_h, pch=19, col="black", cex=.8)
segments(x, b_h - qnorm(.750) * se_h, x, b_h + qnorm(.750) * se_h, lwd=2)
segments(x, b_h - qnorm(.975) * se_h, x, b_h + qnorm(.975) * se_h, lwd=.9)

b_s <- B_s$extremism_maj#[-15]
se_s <- SE_s$extremism_maj#[-15]
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Senate", cex.main=1.15, line=0.75, font.main=2)
points(x, b_s, pch=19, col="black", cex=.8)
segments(x, b_s - qnorm(.750) * se_s, x, b_s + qnorm(.750) * se_s, lwd=2)
segments(x, b_s - qnorm(.975) * se_s, x, b_s + qnorm(.975) * se_s, lwd=.9)

b_h <- B_h$extremism_min#[-12]
se_h <- SE_h$extremism_min#[-12]
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="Congress", ylab="Minority")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b_h, pch=19, col="black", cex=.8)
segments(x, b_h - qnorm(.750) * se_h, x, b_h + qnorm(.750) * se_h, lwd=2)
segments(x, b_h - qnorm(.975) * se_h, x, b_h + qnorm(.975) * se_h, lwd=.9)

b_s <- B_s$extremism_min#[-15]
se_s <- SE_s$extremism_min#[-15]
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="Congress", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b_s, pch=19, col="black", cex=.8)
segments(x, b_s - qnorm(.750) * se_s, x, b_s + qnorm(.750) * se_s, lwd=2)
segments(x, b_s - qnorm(.975) * se_s, x, b_s + qnorm(.975) * se_s, lwd=.9)

dev.off()
