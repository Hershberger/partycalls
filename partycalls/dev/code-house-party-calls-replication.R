library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

# set.seed(1584882915)
# code_party_calls_by_congress_number <- function(congress_number)
# {
#   cat("**** working on house", congress_number, "\n")
#   rc <- get(paste0("h", sprintf("%03.f", congress_number)))
#   rc <- code_party_calls(rc, tval_threshold = 2.32,
#     count_min = 10,
#     count_max = 50, match_count_min = 5, sim_annealing = FALSE,
#     random_seed = FALSE, lopside_thresh = 0.65,
#     drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
#     n_iterations_for_coding = 5, use_new_match_check = FALSE)
# }
# house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
# names(house_party_calls) <- paste0("hou", 93:109)
# save(house_party_calls, file = "inst/extdata/house_party_calls.RData")
load("inst/extdata/house_party_calls.RData")

new_partycalls <- rbindlist(lapply(house_party_calls, function(x) data.table(
    congress = gsub("[A-Za-z:/\\.]", "", x$source),
    voteno = x$party_call_coding$voteno,
    new_coding = x$party_call_coding$coding)))
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress < 110, .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

merged_votes <- merge(old_partycalls, new_partycalls,
  by = c("congress", "voteno"), all = TRUE)
tab1 <- merged_votes[, table(old_coding, new_coding)]
tab2 <- merged_votes[congress != 104, table(old_coding, new_coding)]
chisq.test(tab1)
chisq.test(tab2)
merged_votes[, chisq.test(table(old_coding, new_coding))$stat, congress]
diag_prop <- function(x) sum(diag(x)) / sum(x)
merged_votes[congress != 104, diag_prop(table(old_coding, new_coding))]
merged_votes[congress != 104, diag_prop(table(old_coding, new_coding)), congress]
merged_votes[congress == 102, table(old_coding, new_coding)]
merged_votes[congress == 103, table(old_coding, new_coding)]
merged_votes[congress == 109, table(old_coding, new_coding)]
merged_votes[congress == 96, table(old_coding, new_coding)]
merged_votes[congress == 98, table(old_coding, new_coding)]


new_ideal_points <- rbindlist(lapply(c(93:103, 104:109), function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, house_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsr = icpsrLegis, new_ideal_point = pf_ideal)]
}))

whoheeds13 <- readstata13::read.dta13(
  "inst/extdata/who-heeds-replication-archive.dta")
setDT(whoheeds13)
old_ideal_points <- whoheeds13[,
  .(congress, icpsr, old_ideal_point = ideal_partyfree)]

merged_ideal_points <- merge(old_ideal_points, new_ideal_points,
  by = c("congress", "icpsr"), all = TRUE)
merged_ideal_points[, cor(old_ideal_point, new_ideal_point, use = "p")]
merged_ideal_points[, cor(old_ideal_point, new_ideal_point, use = "p"), congress]

library(ggplot2)
ggplot(merged_ideal_points, aes(old_ideal_point, new_ideal_point)) +
  geom_point(alpha = .1) + coord_equal() + facet_wrap(~congress)

new_responsiveness <- rbindlist(lapply(93:109, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, house_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsr = icpsrLegis, new_ideal_point = pf_ideal,
    new_pirate100 = 100 * responsiveness_party_calls,
    new_pfrate100 = 100 * responsiveness_noncalls,
    new_distance_from_floor_median = dist_from_floor_median,
    new_ideological_extremism = ideological_extremism)]
}))
new_whoheeds13 <- merge(whoheeds13, new_responsiveness,
  by = c("congress", "icpsr"), all = TRUE)



## copy/pasted code from 2013 paper is below (with changed out dataset name
## and variable names)


# Regression formulas and functions
f.meddist <- new_pirate100 ~ new_distance_from_floor_median +
  new_pfrate100 + dpres + south + votepct + female + afam + latino + seniority +
  freshman + retiree + bestgrosswart + leader + power + speaker + chair
f.extremism <- new_pirate100 ~ new_ideological_extremism +
  new_pfrate100 + dpres + south + votepct + female + afam + latino + seniority +
  freshman + retiree + bestgrosswart + leader + power + speaker + chair

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

pdf(file="plots/replicate-who-heeds-figure2.pdf",
  width=8, height = 8, family="Times")
layout(matrix(1:4, 2, 2, byrow=TRUE))
par(mar=c(2.5, 4, 2, 0.3) + 0.1, font.lab=2)

x <- (93:109)[-12]
x.ticks <- c(94, 99, 104, 109)
y.ticks <- c(-12, 0, 12)

b <- B$meddist.maj[-12]
se <- SE$meddist.maj[-12]
plot(0, 0, type='n', ylim=c(-6, 18), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Majority Party")
axis(1, x.ticks, cex.axis=1.1, labels=FALSE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Distance from Floor Median", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism.maj[-12]
se <- SE$extremism.maj[-12]
plot(0, 0, type='n', ylim=c(-6, 18), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=FALSE)
axis(2, y.ticks, cex.axis=1.1, labels=FALSE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Ideological Extremism", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$meddist.min[-12]
se <- SE$meddist.min[-12]
plot(0, 0, type='n', ylim=c(-6, 18), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Minority Party")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism.min[-12]
se <- SE$extremism.min[-12]
plot(0, 0, type='n', ylim=c(-6, 18), xlim=c(93, 109),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=FALSE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b-qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b-qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

dev.off()
