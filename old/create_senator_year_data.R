library(partycalls)
library(yaml)

load("inst/extdata/senate93-112.RData")
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

make_senator_time_invariant_covariates <- function() {
  syd_list <- lapply(93:112, get_initial_senator_data)
  senator_year_data <- rbindlist(syd_list)
  setnames(senator_year_data, "state", "stabb")
  # clean mc tags
  senator_year_data[mc == "DAMATO (R NY)", mc := "D'AMATO (R NY)"]
  senator_year_data[mc == "CHAFEE (R RI)" & icpsrLegis == "49905",
    mc := "CHAFEE (R RI) 49905"]
  senator_year_data[mc == "BAYH (D IN)" & icpsrLegis == "10800",
    mc := "BAYH (D IN) 10800"]
  senator_year_data[mc == "BAYH (D IN)" & icpsrLegis == "49901",
    mc := "BAYH (D IN) 49901"]
  senator_year_data[mc == "BENNETT (R UT)" & icpsrLegis == "645",
    mc := "BENNETT (R UT) 645"]
  senator_year_data[mc == "BENNETT (R UT)" & icpsrLegis == "49307",
    mc := "BENNETT (R UT) 49307"]
  senator_year_data[mc == "PRYOR (D AR)" & icpsrLegis == "10791",
    mc := "PRYOR (D AR) 10791"]
  senator_year_data[mc == "PRYOR (D AR)" & icpsrLegis == "40301",
    mc := "PRYOR (D AR) 40301"]
  senator_year_data[mc == "MURKOWSKI (R AK)" & icpsrLegis == "14907",
    mc := "MURKOWSKI (R AK) 14907"]
  senator_year_data[mc == "MURKOWSKI (R AK)" & icpsrLegis == "40300",
    mc := "MURKOWSKI (R AK) 40300"]

}
