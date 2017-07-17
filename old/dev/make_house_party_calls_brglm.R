library(partycalls)

set.seed(1189028224)

house_party_calls_brglm <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "house", pval_threshold = 0.01, type = "brglm")

names(house_party_calls_brglm) <- paste0("hou", 93:112)
save(house_party_calls_brglm, file = "test_data/house_party_calls_brglm.RData")
