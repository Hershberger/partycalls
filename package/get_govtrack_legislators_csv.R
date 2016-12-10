library(data.table)
library(magrittr)
legislators <-
  c("current", "historic") %>%
  paste0("https://www.govtrack.us/data/congress-legislators/legislators-", .,
    ".csv") %>%
  lapply(read.csv) %>%
  rbindlist
legislators[last_name %like% "Burdick" & first_name %like% "Jocelyn",
  icpsr_id := 49103]
setnames(legislators, c("icpsr_id", "party"), c("icpsrLegis", "partyname"))
legislators[, partyname := NULL]
save(legislators, file = "inst/extdata/legislators.RData")
