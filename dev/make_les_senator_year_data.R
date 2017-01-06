library(partycalls)

les_senate <- fread("inst/extdata/93_113_senate_variables.csv")

# load("inst/extdata/senate93-112.RData")
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
setDT(senator_year_data)

# make les data into a data.table
setDT(les_senate)
# remove unused variables/congresses from les data
les_senate <- les_senate[congress != 113, ]
les_senate[, c("thomas_num", "thomas_name", "st_name", "votepct_sq", "meddist",
  "majdist", "speaker", "power", "budget", "state_leg_prof", "ss_bills",
  "ss_aic", "ss_abc", "ss_pass", "ss_law", "s_bills", "s_aic", "s_abc",
  "s_pass", "s_law", "c_bills", "c_aic", "c_abc", "c_pass", "c_law",
  "all_bills", "all_aic", "all_abc", "all_pass", "all_law", "les",
  "leslag") := NULL]

les_icpsr <- les_senate$icps
senator_year_data[, in_les_data := 1 * (icpsrLegis %in% les_icpsr)]
senator_year_data[in_les_data == 0, ]

les_senate[, freshman_congress := floor(calc_congress(elected))]

senator_year_data <- merge(senator_year_data, les_senate,
  by.x = c("icpsrLegis", "congress"), by.y = c("icpsr", "congress"),
  all.x = TRUE)
senator_year_data[party == "D" & dem == 0, ]
senator_year_data[party != "D" & dem == 1, ]

# correct values in senate data

# female
senator_year_data[icpsrLegis == 14516 & congress == 95, female := 1]
senator_year_data[icpsrLegis == 49103 & congress == 102, female := 1]
senator_year_data[icpsrLegis == 15011 & congress == 104, female := 1]
senator_year_data[icpsrLegis == 15011 & congress == 109, female := 1]
senator_year_data[icpsrLegis == 49300 & congress == 104, female := 1]
senator_year_data[icpsrLegis == 49300 & congress == 109, female := 1]
senator_year_data[icpsrLegis == 14440 & congress == 105, female := 1]
senator_year_data[icpsrLegis == 29732 & congress == 107, female := 1]
senator_year_data[icpsrLegis == 29732 & congress == 109, female := 1]
senator_year_data[icpsrLegis == 49308 & congress == 108, female := 1]
senator_year_data[icpsrLegis == 29740 & congress == 108, latino := 1]
senator_year_data[icpsrLegis == 29740 & congress == 109, latino := 1]
senator_year_data[icpsrLegis == 29740 & congress == 110, latino := 1]
senator_year_data[icpsrLegis == 40500 & congress == 109, latino := 1]
senator_year_data[icpsrLegis == 40500 & congress == 110, latino := 1]

# TODO HERSHBERGER: CHECK YEAR ELECTED; BOXER (ICPSRLEGIS 15011) SEEMS WONKY
freshman_dt <- senator_year_data[, .(
  freshman_congress = mean(freshman_congress),
  elected = mean(elected)
), .(icpsrLegis)]
