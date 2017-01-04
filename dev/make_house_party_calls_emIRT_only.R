library(partycalls)
set.seed(1975242355, kind = "L'Ecuyer")

house_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "house", sim_annealing = FALSE, use_new_match_check = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(house_party_calls,
  file = "test_data/house_party_calls_emIRT_only.RData")
