library(partycalls)

set.seed(703851427)

house_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "house", pval_threshold = 0.01, type = "lm")

save(house_party_calls, file = "test_data/house_party_calls_lm.RData")
