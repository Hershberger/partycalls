library(partycalls)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")
h112 <- pscl::readKH("inst/extdata/hou112kh.ord")

set.seed(1584882915)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  code_party_calls(rc)
}
house_party_calls <- lapply(93:112, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:112)
save(house_party_calls, file = "inst/extdata/house_party_calls.RData")
