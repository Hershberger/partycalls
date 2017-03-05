library(partycalls)

set.seed(545040179)

house_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "house", pval_threshold = 0.01, very_lopsided_threshold = 4,
  type = "lm")

save(house_party_calls,
  file = "test_data/house_party_calls_lm_drop_lopsided.RData")
