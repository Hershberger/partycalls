library(partycalls)
set.seed(1975242355)
house_party_calls_replication_emIRT_only <- lapply(93:109,
  code_party_calls_by_congress_number,
  count_max = 100, match_count_min = 10, sim_annealing = FALSE,
  drop_very_lopsided_votes = TRUE, use_new_match_check = FALSE)
save(house_party_calls_replication_emIRT_only,
  file = "test_data/house_party_calls_replication_emIRT_only_v2.RData")
