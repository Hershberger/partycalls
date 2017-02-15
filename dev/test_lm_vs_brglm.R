library(partycalls)

party_calls_brglm <- lapply(93:112,
  partycalls::code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05,  type = "brglm")


party_calls_lm <- lapply(93:112,
  partycalls::code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05,  type = "lm")


# make 2x2 for lopsided/close vs. calls/noncalls for
# 1. brglm
# 2. lm
