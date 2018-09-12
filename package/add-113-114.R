# add 113, 114 to party calls ----
library(partycalls)
library(pscl)
library(data.table)
library(readstata13)

votes <- read.csv("~/Downloads/H113_votes.csv")
legis_data <- read.csv("~/Downloads/H113_members.csv")
names(legis_data) <- gsub("state_abbrev", "state", names(legis_data))
legis_data$party <- ifelse(legis_data$party_code == 100, "D", "R")
vote_data <- read.csv("~/Downloads/H113_rollcalls.csv")
vote_matrix <- matrix(NA, nrow = nrow(legis_data), ncol = nrow(vote_data))
ii <- match(votes[, 4], legis_data$icpsr)
jj <- match(votes[, 3], vote_data$rollnumber)
vote_matrix[cbind(ii, jj)] <- votes[, 5]
h113 <- rollcall(vote_matrix,
  yea = 1, nay = 6:7, missing = NA, notInLegis = 9,
  legis.names = NULL, vote.names = NULL,
  legis.data = legis_data, vote.data = vote_data,
  desc = NULL, source = NULL)

votes <- read.csv("~/Downloads/H114_votes.csv")
legis_data <- read.csv("~/Downloads/H114_members.csv")
names(legis_data) <- gsub("state_abbrev", "state", names(legis_data))
legis_data$party <- ifelse(legis_data$party_code == 100, "D", "R")
vote_data <- read.csv("~/Downloads/H114_rollcalls.csv")
vote_matrix <- matrix(NA, nrow = nrow(legis_data), ncol = nrow(vote_data))
ii <- match(votes[, 4], legis_data$icpsr)
jj <- match(votes[, 3], vote_data$rollnumber)
vote_matrix[cbind(ii, jj)] <- votes[, 5]
h114 <- rollcall(vote_matrix,
  yea = 1, nay = 6:7, missing = NA, notInLegis = 9,
  legis.names = NULL, vote.names = NULL,
  legis.data = legis_data, vote.data = vote_data,
  desc = NULL, source = NULL)

set.seed(703851427)
hou113 <- code_party_calls_by_congress_number(113, chamber = "house",
  pval_threshold = 0.005, type = "lm")

set.seed(1980693245)
hou114 <- code_party_calls_by_congress_number(114, chamber = "house",
  pval_threshold = 0.0005, type = "lm")

devtools::use_data(hou113, hou114, overwrite = TRUE)

# setup environment ----

get_legis_data <- function(rc) {
  ld <- rc$legis.data
  ld$mc <- rownames(ld)
  setDT(ld)
  votes <- rc$votes
  votes <- melt(votes)
  data.table::setDT(votes)
  setnames(votes, c("mc", "vote_id", "vote"))
  ld[, mc := paste("Legislator", .I + 1)]
  votes <- merge(votes, ld, by = "mc")
  party_call_coding <- rc$party_call_coding$coding
  noncalls <- votes$vote_id[which(party_call_coding == "noncall")]
  votes[, `:=`(noncall, as.numeric(vote_id %in% noncalls))]
  p <- emIRT::makePriors(rc$n, rc$m, 1)
  s <- emIRT::getStarts(rc$n, rc$m, 1)
  fitted_emIRT_all <- emIRT::binIRT(.rc = rc, .starts = s,
    .priors = p, .control = list(threads = 1, verbose = FALSE,
      thresh = 1e-06))

  rc_noncalls <- rc
  votes_to_keep <- which(colnames(rc_noncalls$votes) %in% noncalls)
  rc_noncalls$votes <- rc_noncalls$votes[, votes_to_keep]
  rc_noncalls$m <- ncol(rc_noncalls$votes)
  p <- emIRT::makePriors(rc_noncalls$n, rc_noncalls$m, 1)
  s <- emIRT::getStarts(rc_noncalls$n, rc_noncalls$m, 1)
  fitted_emIRT <- emIRT::binIRT(.rc = rc_noncalls, .starts = s,
    .priors = p, .control = list(threads = 1, verbose = FALSE,
      thresh = 1e-06))
  ideal <- as.data.table(fitted_emIRT$means$x)
  ideal[, party := rc_noncalls$legis.data$party]
  orientation_correct <- mean(ideal[party == "D", d1]) < mean(ideal[party ==
      "R", d1])
  if (!orientation_correct) {
    ideal[, d1 := -d1]
  }
  ideal[, icpsrLegis := ld$icpsr]
  ideal[, nominate_dim1 := rc_noncalls$legis.data$nominate_dim1]
  ideal[, d1_all := fitted_emIRT_all$means$x]
  ideal
}

lep_data_93_113 <-
  readstata13::read.dta13("inst/extdata/LEP93to113.dta")
lep_data_114 <-
  readstata13::read.dta13("~/dropbox/factioncalls/data/LEP114CleanNeedsMoreCovariates.dta")
setDT(lep_data_93_113)
setDT(lep_data_114)

lep_data_113_114 <- rbind(
  lep_data_93_113[congress==113,
    intersect(names(lep_data_93_113), names(lep_data_114)), with = FALSE],
  lep_data_114[,
    intersect(names(lep_data_93_113), names(lep_data_114)), with = FALSE]
)

# merge  ----

rc113 <- get_legis_data(hou113)
rc114 <- get_legis_data(hou114)
rc113 <- merge(
  rc113[, .(congress = 113, icpsr = icpsrLegis, party_free_ideal_point = d1,
    dem = as.numeric(party == "D"))],
  lep_data_113_114[congress == 113])
rc114 <- merge(
  rc114[, .(congress = 114, icpsr = icpsrLegis, party_free_ideal_point = d1,
    dem = as.numeric(party == "D"))],
  lep_data_114)

rc113 <- rc113[, names(house_data), with = FALSE]

setdiff(names(rc113), names(house_data))
setdiff(names(house_data), names(rc113))

setdiff(names(house_data), names(rc114))
setdiff(names(rc114), names(house_data))

setcolorder(rc113, names(house_data))
setcolorder(rc114, names(house_data))

house_data <- rbind(house_data, rc113, rc114)

save(house_data, file = "data/house_data.RData")


# make house_party_calls ----
house_party_calls <- vector(mode = "list", length = 11)
for (i in 1:9) {
  house_party_calls[[i]] <- partycalls::house_party_calls[[11 + i]]
}
house_party_calls[[10]] <- hou113
house_party_calls[[11]] <- hou114

save(house_party_calls, file = "data/house_party_calls.RData")
