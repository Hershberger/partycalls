library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-6.R")
load("inst/extdata/houKHfiles001-111.rdata")

set.seed(1975242355)
house_party_calls_103_seed1 <- code_party_calls(h103,
  randomly_reassign_flip_flop_votes_from_noncalls = TRUE,
  count_min = 50,
  match_count_min = 5,
  sim_annealing = FALSE)

set.seed(5532425791)
house_party_calls_103_seed2 <- code_party_calls(h103,
  randomly_reassign_flip_flop_votes_from_noncalls = TRUE,
  count_min = 50,
  match_count_min = 5,
  sim_annealing = FALSE)
