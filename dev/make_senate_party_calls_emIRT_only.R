library(partycalls)
set.seed(1975242355, kind = "L'Ecuyer")

senate_party_calls <- lapply(93, code_party_calls_by_congress_number,
  chamber = "senate", sim_annealing = FALSE, use_new_match_check = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(senate_party_calls,
  file = "test_data/senate_party_calls_emIRT_only.RData")
