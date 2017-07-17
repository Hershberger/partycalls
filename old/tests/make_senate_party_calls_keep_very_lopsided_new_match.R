library(partycalls)
set.seed(347611200, kind = "L'Ecuyer")

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05, sim_annealing = FALSE,
  use_new_match_check = TRUE, drop_very_lopsided_votes = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(senate_party_calls,
  file = "test_data/senate_party_calls_keep_very_lopsided.RData")
