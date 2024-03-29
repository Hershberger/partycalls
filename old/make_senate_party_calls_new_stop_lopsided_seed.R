library(partycalls)
set.seed(271724931, kind = "L'Ecuyer")

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05, sim_annealing = FALSE,
  use_new_match_check = TRUE, random_seed = FALSE, semi_random_seed = FALSE,
  vote_switch_percent = 0.01, initial_vote_switch_pct = 0,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(senate_party_calls,
  file = "test_data/senate_party_calls_new_stop_lopsided_seed.RData")
