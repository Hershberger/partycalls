library(partycalls)
set.seed(323771536, kind = "L'Ecuyer")

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05, sim_annealing = FALSE,
  use_new_match_check = TRUE, random_seed = FALSE, semi_random_seed = TRUE,
  vote_switch_percent = 0.005, initial_vote_switch_pct = 0.5,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(senate_party_calls,
  file = "test_data/senate_party_calls_new_stop2_semi_random_seed.RData")
