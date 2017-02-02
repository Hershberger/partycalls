library(partycalls)
set.seed(952372552, kind = "L'Ecuyer")

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05, sim_annealing = FALSE,
  use_new_match_check = FALSE, semi_random_seed = TRUE,
  initial_vote_switch_pct = 0.25, hybrid = FALSE, reassign_flip_flop = FALSE)

save(senate_party_calls,
  file = "test_data/senate_party_calls_semirandom_25.RData")
