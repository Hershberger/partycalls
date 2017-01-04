library(partycalls)
library(yaml)

load("inst/extdata/senate93-112.RData")
les_senate <- read.csv("inst/extdata/93_113_senate_variables.csv")

setnames(les_senate, "st_name", "stabb")
setnames(les_senate, "")

get_initial_senator_data <- function(congress) {
  rc <- get(paste0("sen", congress))
  ld <- rc$legis.data
  ld$mc <- rownames(ld)
  setDT(ld)
  ld[, congress := congress]
  # remove presidents
  ld[state != "USA", .(mc, congress, state, icpsrState, icpsrLegis, party,
    partyCode)]
}

syd_list <- lapply(93:112, get_initial_senator_data)
senator_year_data <- rbindlist(syd_list)
setnames(senator_year_data, "state", "stabb")
