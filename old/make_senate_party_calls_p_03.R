library(partycalls)
set.seed(1627374029, kind = "L'Ecuyer")

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.03, sim_annealing = FALSE, use_new_match_check = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(senate_party_calls,
  file = "test_data/senate_party_calls_p_03.RData")
