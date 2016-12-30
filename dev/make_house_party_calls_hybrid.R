library(partycalls)
set.seed(1209003941, kind = "L'Ecuyer")

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number,
  chamber = "house", sim_annealing = TRUE, use_new_match_check = TRUE,
  hybrid = TRUE, reassign_flip_flop = FALSE)

save(house_party_calls,
  file = "test_data/house_party_calls_hybrid.RData")
