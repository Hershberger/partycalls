library(partycalls)
set.seed(1209003941)

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number,
  hybrid = TRUE)

save(house_party_calls,
  file = "test_data/house_party_calls_replication_hybrid.RData")
