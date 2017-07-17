library(partycalls)

set.seed(2081425373)

senate_party_calls_brglm <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05, type = "brglm")

names(senate_party_calls_brglm) <- paste0("sen", 93:112)
save(senate_party_calls_brglm, file = "test_data/senate_party_calls_brglm.RData")
