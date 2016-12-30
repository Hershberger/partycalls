library(partycalls)
set.seed(535553638, kind = "L'Ecuyer")

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number,
  chamber = "house", sim_annealing = FALSE, use_new_match_check = TRUE,
  hybrid = FALSE, reassign_flip_flop = TRUE)

save(house_party_calls,
  file = "test_data/house_party_calls_reassign_flip_flop.RData")
