library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-7.R")
load("inst/extdata/houKHfiles001-111.rdata")
set.seed(1975242355)

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number,
  hybrid = TRUE)

save(house_party_calls, file = "test_data/house_party_calls_replication_hybrid.RData")
