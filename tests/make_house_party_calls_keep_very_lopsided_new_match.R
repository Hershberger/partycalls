library(partycalls)
set.seed(93789108, kind = "L'Ecuyer")

house_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "house", sim_annealing = FALSE, use_new_match_check = TRUE,
  drop_very_lopsided_votes = FALSE,
  hybrid = FALSE, reassign_flip_flop = FALSE)

save(house_party_calls,
  file = "test_data/house_party_calls_keep_very_lopsided_new_match.RData")
