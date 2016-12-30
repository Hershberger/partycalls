library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-6.R")
load("inst/extdata/houKHfiles001-111.rdata")
set.seed(1975242355)
house_party_calls <- lapply(93:109, code_party_calls_by_congress_number,
  randomly_reassign_flip_flop_votes_from_noncalls = FALSE)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls,
  file = "test_data/house_party_calls_annealing_seed1.RData")
