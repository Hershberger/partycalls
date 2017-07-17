library(partycalls)

set.seed(846492672)

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05, type = "lm")

names(senate_party_calls) <- paste0("sen", 93:112)
save(senate_party_calls, file = "test_data/senate_party_calls_lm.RData")
