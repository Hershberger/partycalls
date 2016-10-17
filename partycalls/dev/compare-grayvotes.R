library(partycalls)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

code_party_calls_by_congress_number_house <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", congress_number))
  code_party_calls(rc)
}
house_party_calls <- lapply(sprintf("%03d", 93:111),
  code_party_calls_by_congress_number_house)
names(house_party_calls) <- paste0("h", sprintf("%03d", 93:111))
