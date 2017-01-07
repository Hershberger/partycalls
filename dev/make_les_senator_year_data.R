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
  "leslag", "seniority", "sensq") := NULL]

les_icpsr <- les_senate$icpsr
senator_year_data[, in_les_data := 1 * (icpsrLegis %in% les_icpsr)]
# see if anyone is missing
senator_year_data[in_les_data == 0, ]

# merge in les variables
senator_year_data <- merge(senator_year_data, les_senate,
  by.x = c("icpsrLegis", "congress"), by.y = c("icpsr", "congress"),
  all.x = TRUE)
# see if party is miscoded for anyone
senator_year_data[party == "D" & dem == 0, ]
senator_year_data[party != "D" & dem == 1, ]

# correct elected variable
freshman_dt <- senator_year_data[, .(
  elected_mean = mean(elected),
  elected_mean_no_NA = mean(elected, na.rm = TRUE)
), .(icpsrLegis)]
senator_year_data <- merge(senator_year_data, freshman_dt, by = "icpsrLegis")
freshman_congress_check <-
  senator_year_data[elected != elected_mean, ]
freshman_congress_check2 <-
  senator_year_data[elected != elected_mean_no_NA]
unique(freshman_congress_check$icpsrLegis)

# fixing mcs with mistakes in year elected

# MILTON YOUNG
senator_year_data[icpsrLegis == 10450 & congress >= 78, elected := 1945]
# ROBERT STAFFORD
senator_year_data[icpsrLegis == 10562 & congress >= 92, elected := 1971]
# BROCK ADAMS
senator_year_data[icpsrLegis == 10700 & congress >= 100, elected := 1986]
# HARRY BYRD, JR.
senator_year_data[icpsrLegis == 10802 & congress >= 89, elected := 1965]
# WALTER MONDALE
senator_year_data[icpsrLegis == 10813 & congress >= 88, elected := 1964]
# DEWEY BARTLETT
senator_year_data[icpsrLegis == 14100 & congress >= 93, elected := 1972]
# SAM NUNN
senator_year_data[icpsrLegis == 14108 & congress >= 93, elected := 1972]
# ORRIN HATCH
senator_year_data[icpsrLegis == 14503 & congress >= 95, elected := 1976]
# DICK LUGAR
senator_year_data[icpsrLegis == 14506 & congress >= 95, elected := 1976]
# DAN COATS
senator_year_data[icpsrLegis == 14806 & congress >= 101 & congress <= 105,
  elected := 1988]
# BARBARA BOXER
senator_year_data[icpsrLegis == 15011 & congress >= 103, elected := 1992]
# DIANE FEINSTEIN
senator_year_data[icpsrLegis == 49300 & congress >= 103, elected := 1992]
# ROBERT BENNETT
senator_year_data[icpsrLegis == 49307 & congress >= 103, elected := 1992]
# EVAN BAYH
senator_year_data[icpsrLegis == 49901 & congress >= 106, elected := 1998]

# THOMAS GORTON is correct as is
# FRANK LAUTENBERG is correct as is

senator_year_data[, freshman_congress := ceiling(calc_congress(elected + 1))]

# correct more values in senate data

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

# CORRECT FRESHMAN CONGRESS FOR APPOINTEES
syd_93 <- senator_year_data[congress == 93, ]
les_93 <- les_senate[congress == 93, ]
syd_93[, in_les := 1 * (icpsrLegis %in% les_93$icpsr)]
syd_93[in_les == 0, ]

# WILL NEED TO ADD AND CORRECT CLASS
class_1 <- c(95, 98, 101, 104, 107, 110, 113)
class_2 <- c(93, 96, 99, 102, 105, 108, 111)
class_3 <- c(94, 97, 100, 103, 106, 109, 112)

senator_year_data[freshman_congress %in% class_1, class := 1]
senator_year_data[freshman_congress %in% class_2, class := 2]
senator_year_data[freshman_congress %in% class_3, class := 3]

# TODO HERSHBERGER: MAKE SURE FRESHMAN CONGRESS IS CORRECT FOR SPECIAL ELECTIONS
# ADD IN SUPERFRESHMAN VARIABLE ONCE THAT OTHER STUFF IS CORRECT
# FIX SPECIAL ELECTIONS
# MAKE SURE APPOINTEES DON'T HAVE votepct VALUES
