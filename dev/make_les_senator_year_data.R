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

# MURIEL HUMPHREY
senator_year_data[icpsrLegis == 14516 & congress == 95, female := 1]
# JOCELYN BURDICK
senator_year_data[icpsrLegis == 49103 & congress == 102, female := 1]
# BARBARA BOXER
senator_year_data[icpsrLegis == 15011 & congress == 104, female := 1]
senator_year_data[icpsrLegis == 15011 & congress == 109, female := 1]
# DIANE FEINSTEIN
senator_year_data[icpsrLegis == 49300 & congress == 104, female := 1]
senator_year_data[icpsrLegis == 49300 & congress == 109, female := 1]
# BARBARA MIKULSKI
senator_year_data[icpsrLegis == 14440 & congress == 105, female := 1]
# DEBBIE STABENOW
senator_year_data[icpsrLegis == 29732 & congress == 107, female := 1]
senator_year_data[icpsrLegis == 29732 & congress == 109, female := 1]
# PATTY MURRAY
senator_year_data[icpsrLegis == 49308 & congress == 108, female := 1]

# latino

# JOHN SUNUNU
senator_year_data[icpsrLegis == 29740 & congress == 108, latino := 1]
senator_year_data[icpsrLegis == 29740 & congress == 109, latino := 1]
senator_year_data[icpsrLegis == 29740 & congress == 110, latino := 1]
# KEN SALAZAR
senator_year_data[icpsrLegis == 40500 & congress == 109, latino := 1]
senator_year_data[icpsrLegis == 40500 & congress == 110, latino := 1]

# TODO HERSHBERGER: CHECK YEAR ELECTED; BOXER (ICPSRLEGIS 15011) SEEMS WONKY
freshman_dt <- senator_year_data[, .(
  freshman_congress_mean = mean(freshman_congress),
  elected_mean = mean(elected)
), .(icpsrLegis)]
senator_year_data <- merge(senator_year_data, freshman_dt, by = "icpsrLegis")
freshman_congress_check <-
  senator_year_data[freshman_congress != freshman_congress_mean, ]
unique(freshman_congress_check$icpsrLegis)

# fixing mcs with mistakes in year elected

# MILTON YOUNG
senator_year_data[icpsrLegis == 10450 & congress >= 78, elected := 1945,
  freshman_congress := 78]
# ROBERT STAFFORD
senator_year_data[icpsrLegis == 10562 & congress >= 92, elected := 1971,
  freshman_congress := 92]
# BROCK ADAMS
senator_year_data[icpsrLegis == 10700 & congress >= 100, elected := 1986,
  freshman_congress := 100]
# HARRY BYRD, JR.
senator_year_data[icpsrLegis == 10802 & congress >= 89, elected := 1965,
  freshman_congress := 89]
# WALTER MONDALE
senator_year_data[icpsrLegis == 10813 & congress >= 88, elected := 1964,
  freshman_congress := 88]
# DEWEY BARTLETT
senator_year_data[icpsrLegis == 14100 & congress >= 93, elected := 1972,
  freshman_congress := 93]
# SAM NUNN
senator_year_data[icpsrLegis == 14108 & congress >= 92, elected := 1972,
  freshman_congress := 93]


# TODO HERSHBERGER: MAKE SURE FRESHMAN CONGRESS CALCULATED CORRECTLY
# MIGHT BE ONE YEAR TOO LOW
