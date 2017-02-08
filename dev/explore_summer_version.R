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
