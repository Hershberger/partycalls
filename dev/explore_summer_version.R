library(data.table)
library(emIRT)

library()


source("dev/summer_version.R")
set.seed(201375487)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on senate", congress_number, "\n")
  rc <- get(paste0("sen", congress_number))
  code_party_calls(rc)
}
load("data/sen93.rda")
# debug(code_party_calls)
party_calls_93_summer <- code_party_calls_by_congress_number(93)

party_calls_93_winter <- partycalls::code_party_calls_by_congress_number(93,
  chamber = "senate",
  pval_threshold = 0.05, sim_annealing = FALSE, use_new_match_check = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE)

length(intersect(party_calls_93_summer$party_calls,
  party_calls_93_winter$party_calls))

length(setdiff(party_calls_93_summer$party_calls,
  party_calls_93_winter$party_calls))

length(setdiff(party_calls_93_winter$party_calls,
  party_calls_93_summer$party_calls))

source("dev/modified_summer_version.R")
party_calls_93_summer_modified <- code_party_calls_by_congress_number(93)

yea_perc <- apply(party_calls_93_summer_modified$votes, 2,
  function(x) sum(x == 1, na.rm = TRUE) / sum(x %in% c(1, -1) , na.rm = TRUE))
lopsided <- 1 * (yea_perc <= .35 | yea_perc >= .65)
partycalls <- rep(0, length(yea_perc))
partycalls[party_calls_93_summer_modified$party_calls] <- 1
table(lopsided, partycalls)

length(intersect(party_calls_93_summer_modified$party_calls,
  party_calls_93_winter$party_calls))

length(setdiff(party_calls_93_summer_modified$party_calls,
  party_calls_93_winter$party_calls))

length(setdiff(party_calls_93_winter$party_calls,
  party_calls_93_summer_modified$party_calls))


length(intersect(party_calls_93_summer_modified$party_calls,
  party_calls_93_summer$party_calls))

length(setdiff(party_calls_93_summer_modified$party_calls,
  party_calls_93_summer$party_calls))

length(setdiff(party_calls_93_summer$party_calls,
  party_calls_93_summer_modified$party_calls))


load("data/sen112.rda")
source("dev/modified_summer_version.R")
party_calls_112_summer_modified <- code_party_calls_by_congress_number(112)

yea_perc <- apply(party_calls_112_summer_modified$votes, 2,
  function(x) sum(x == 1, na.rm = TRUE) / sum(x %in% c(1, -1) , na.rm = TRUE))
lopsided <- 1 * (yea_perc <= .35 | yea_perc >= .65)
partycalls <- rep(0, length(yea_perc))
partycalls[party_calls_112_summer_modified$party_calls] <- 1
table(lopsided, partycalls)


load("data/sen111.rda")
source("dev/modified_summer_version.R")
party_calls_111_summer_modified <- code_party_calls_by_congress_number(111)

yea_perc <- apply(party_calls_111_summer_modified$votes, 2,
  function(x) sum(x == 1, na.rm = TRUE) / sum(x %in% c(1, -1) , na.rm = TRUE))
lopsided <- 1 * (yea_perc <= .35 | yea_perc >= .65)
partycalls <- rep(0, length(yea_perc))
partycalls[party_calls_111_summer_modified$party_calls] <- 1
table(lopsided, partycalls)

party_calls_111_winter <- partycalls::code_party_calls_by_congress_number(111,
  chamber = "senate",
  pval_threshold = 0.05, sim_annealing = FALSE, use_new_match_check = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE,
  drop_very_lopsided_votes = TRUE)

yea_perc <- apply(party_calls_111_winter$votes, 2,
  function(x) sum(x == 1, na.rm = TRUE) / sum(x %in% c(1, -1) , na.rm = TRUE))
lopsided <- 1 * (yea_perc <= .35 | yea_perc >= .65)
partycalls <- rep(0, length(yea_perc))
partycalls[party_calls_111_winter$party_calls] <- 1
table(lopsided, partycalls)
