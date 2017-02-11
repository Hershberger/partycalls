load("inst/extdata/senate93-112.RData")
source("dev/modified_summer_version.R")
library(data.table)
library(emIRT)
set.seed(2062845155)

code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on senate", congress_number, "\n")
  rc <- get(paste0("sen", congress_number))
  code_party_calls(rc)
}

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number)

save(senate_party_calls,
  file = "test_data/senate_party_calls_summer_modified.RData")
