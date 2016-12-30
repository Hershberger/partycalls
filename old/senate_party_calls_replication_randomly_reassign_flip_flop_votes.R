library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-7.R")
load("inst/extdata/senate93-112.RData")
set.seed(1975242355)

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", randomly_reassign_flip_flop_votes_from_noncalls = TRUE)



save(senate_party_calls,
  file = "test_data/senate_party_calls_replication_randomly_reassign_flip_flop_votes.RData")
