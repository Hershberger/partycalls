options(stringsAsFactors = FALSE)
library(partycalls)
library(yaml)

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

# Clean different mc's for D'Amato
senator_year_data[mc == "DAMATO (R NY)", mc := "D'AMATO (R NY)"]

# Create separate mc tags for parent-child pairs
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

## BEGIN TIME INVARIANT COVARIATES

# Make senator_data
senator_data <- senator_year_data[, .N, .(icpsrLegis, mc, stabb)]

# get govtrack covariates
# source("package/get_govtrack_legislators_csv.R")
load("inst/extdata/legislators.RData")

# get entries for party changers
party_changer_legislators <- c(94240, 14659, 14910, 15407)
party_changer_DT <- legislators[icpsrLegis %in% party_changer_legislators, ]
party_changer_DT[icpsrLegis == 94240, icpsrLegis := 14240]
party_changer_DT[icpsrLegis == 14659, icpsrLegis := 94659]
party_changer_DT[icpsrLegis == 14910, icpsrLegis := 94910]
party_changer_DT[icpsrLegis == 15407, icpsrLegis := 95407]
legislators <- rbind(legislators, party_changer_DT)

# merge in govtrack covariates
senator_data <- merge(senator_data, legislators, by = "icpsrLegis")

# Populate class
# source("package/get_govtrack_legislators_yaml_data.R")
load("inst/extdata/legislators_yaml.RData")
senator_data <- merge(senator_data, legislators_yaml, by = "icpsrLegis")

# Populate years of service
source("package/get_bios.R")
years_of_service_DT <- data.table(bioguide_id = names(bios),
  years_of_service = substr(bios,
    regexpr("Senate Years of Service", bios) + 25, regexpr("Party", bios) - 2))
senator_data <- merge(senator_data, years_of_service_DT, by = "bioguide_id")
senator_data[, years_of_service := gsub(",", ";", years_of_service)]
senator_data[icpsrLegis == 9369, years_of_service := "1954-2003"]
senator_data[icpsrLegis == 10802, years_of_service := "1965-1983"]

# Populate died_in_office, defeated_for_renomination, defeated_for_reelection,
#   changed_party_affiliation, resigned, did_not_seek_reelection
source("package/mine_bios.R")

# Populate different south indicators
south11 <- c("SC", "MS", "FL", "AL", "AR", "GA", "LA", "TX", "VA", "TN", "NC")
south13 <- c(south11, "OK", "KY")
south17 <- c(south13, "DE", "WV", "MD", "MO")
senator_data[, south11 := 1 * (stabb %in% south11)]
senator_data[, south13 := 1 * (stabb %in% south13)]
senator_data[, south17 := 1 * (stabb %in% south17)]

# # END TIME INVARIANT COVARIATES

# Merge TIME-INVARIANT COVARIATES into senator_year_data
senator_year_data <- merge(senator_year_data, senator_data,
    by = c("icpsrLegis", "mc", "stabb"), all.x = TRUE)

# # BEGIN TIME VARYING COVARIATES

# load les data and make into a data.table
les_senate <- fread("inst/extdata/93_113_senate_variables.csv")
# remove missing and unused variables/congresses from les data
les_senate <- les_senate[congress != 113, ]
les_senate[, c("thomas_num", "thomas_name", "st_name", "votepct_sq",
  "meddist", "majdist", "speaker", "power", "budget", "state_leg_prof",
  "ss_bills", "ss_aic", "ss_abc", "ss_pass", "ss_law", "s_bills", "s_aic",
  "s_abc", "s_pass", "s_law", "c_bills", "c_aic", "c_abc", "c_pass", "c_law",
  "all_bills", "all_aic", "all_abc", "all_pass", "all_law", "les",
  "leslag", "seniority", "sensq") := NULL]

# merge in les variables
senator_year_data <- merge(senator_year_data, les_senate,
  by.x = c("icpsrLegis", "congress"), by.y = c("icpsr", "congress"),
  all.x = TRUE)

# fixing mcs with mistakes in year elected
# HUBERT HUMPHREY
senator_year_data[icpsrLegis == 4728 & congress == 95, elected == 1970]
# HENRY HEINZ
senator_year_data[icpsrLegis == 13050 & congress == 102, elected := 1976]
# EDWARD ZORINSKY
senator_year_data[icpsrLegis == 14512 & congress == 100, elected := 1976]
# RON WYDEN
senator_year_data[icpsrLegis == 14871 & congress == 104, elected := 1996]
# ARLEN SPECTER
senator_year_data[icpsrLegis == 14910 & congress == 111, elected := 1980]
# DANIEL EVANS
senator_year_data[icpsrLegis == 14916 & congress == 98, elected := 1983]
# BEN NIGHTHORSE CAMPBELL
senator_year_data[icpsrLegis == 15407 & congress == 104, elected := 1992]
# JAMES INHOFE
senator_year_data[icpsrLegis == 15424 & congress == 103, elected := 1994]
# CRAIG THOMAS
senator_year_data[icpsrLegis == 15633 & congress == 110, elected := 1994]
# MARK KIRK
senator_year_data[icpsrLegis == 20115 & congress == 111, elected := 2010]
# BOB MENENDEZ
senator_year_data[icpsrLegis == 29373 & congress == 109, elected := 2006]
# GEORGE MITCHELL
senator_year_data[icpsrLegis == 14713 & congress == 96, elected := 1980]
# DANIEL AKAKA
senator_year_data[icpsrLegis == 14400 & congress == 101, elected := 1990]
# PAUL LAXALT
senator_year_data[icpsrLegis == 14077 & congress == 93, elected := 1974]
# ROGER WICKER
senator_year_data[icpsrLegis == 29534 & congress == 110, elected := 2007]
# JOHN ENSIGN
senator_year_data[icpsrLegis == 29537 & congress == 112, elected := 2000]
# JOE MANCHIN
senator_year_data[icpsrLegis == 40915 & congress == 111, elected := 2010]
# CHRIS COONS
senator_year_data[icpsrLegis == 40915 & congress == 111, elected := 2010]
# ZELL MILLER
senator_year_data[icpsrLegis == 49904 & congress == 106, elected := 2000]
# LINCOLN CHAFEE
senator_year_data[icpsrLegis == 49905 & congress == 106, elected := 1999]

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
# JAMES BROYHILL is correct as is
# HOWARD METZENBAUM is correct as is
# JOE BIDEN is correct as is
# ROBERT KRUEGER is correct as is
# DAN QUAYLE is correct as is
# KANEASTER HODGES is correct as is
# PAUL HATFIELD is correct as is
# MARYON ALLEN is correct as is
# NICHOLAS BRADY is correct as is
# HILLARY CLINTON is correct as is
# DEAN BARKLEY is correct as is
# KEN SALAZAR is correct as is
# GEORGE LEMIEUX is correct as is
# CARTE GOODWIN is correct as is
# BRIAN SCHATZ is correct as is
# JOCELYN BURDICK is correct as is
# SHEILA FRAHM is correct as is

# Populate year of most recent normal (i.e., on-cycle) election
senator_year_data[,
  year_of_most_recent_normal_election := calc_year_elected(congress, class)]

# Populate on-cycle elections data
source("package/senate_seat_elections.R")
senator_year_data <- merge(senator_year_data, senate_seat_elections,
  by.x = c("year_of_most_recent_normal_election", "stabb", "class"),
  by.y = c("election_year", "stabb", "class"))

# For senators appointed to fill remaining terms, replace election returns
# with data from special elections when available
source("package/fix_special_elections.R")

# here are the times when we use election results from a different
# candidate's previous victory
# Mark these as "drop"
senator_year_data[icpsrLegis == "29373", last_name := "Menendez"]
check <- sapply(1:2041, function(i) {
  !grepl(senator_year_data$last_name[i],
    senator_year_data$candname_vote_rank_1[i], ignore.case = TRUE)
})
check <- senator_year_data[check, .(congress, stabb, icpsrLegis,
  last_name, candname_vote_rank_1)]
senator_year_data[, drop := 0]

remove_from_check <-  c(10514, 3658, 10802, 10562, 4728, 14107, 14904,
  49306, 15502, 29369, 14914)
check[, remove := 0]
check[icpsrLegis %in% remove_from_check, remove := 1]
check[icpsrLegis == 14073 & congress >= 95, remove := 1]
check <- check[remove == 0, ]
check[, remove := NULL]

for (i in 1:nrow(check)) {
  iL <- check$icpsrLegis[i]
  cong <- check$congress[i]
  senator_year_data[icpsrLegis == iL & congress == cong,
    drop := 1]
}

# create party caucus variable
senator_year_data[party == "R", caucus := "Republican"]
senator_year_data[party == "D", caucus := "Democrat"]
senator_year_data[dem == 112, caucus := "Republican"]
senator_year_data[dem == 328, caucus := "Democrat"]
senator_year_data[icpsrLegis == 40106, caucus := "Republican"]

# Populate vote_share
senator_year_data[,
  vote_share := vote_vote_rank_1 / (vote_vote_rank_1 + vote_vote_rank_2)]
senator_year_data[
  party == "R" &
    party_vote_rank_1 == "Democratic" &
    !last_name %in% c("Shelby", "Specter") &
    candname_vote_rank_1 != "Ben Nighthorse Campbell",
  vote_share := vote_vote_rank_2 / (vote_vote_rank_1 + vote_vote_rank_2)]
senator_year_data[
  party == "D" &
    party_vote_rank_1 == "Republican" &
    !last_name %in% c("Shelby", "Specter") &
    candname_vote_rank_1 != "Ben Nighthorse Campbell",
  vote_share := vote_vote_rank_2 / (vote_vote_rank_1 + vote_vote_rank_2)]

# Populate most recent presidential election returns
senator_year_data[, most_recent_presidential_election_year :=
    1784 + 2 * (congress + congress %% 2)]
pres_election_dt <- fread("inst/extdata/pres_election_DT.csv")
senator_year_data <- merge(senator_year_data, pres_election_dt,
  by.x = c("stabb", "most_recent_presidential_election_year"),
  by.y = c("stabb", "election_year"))
senator_year_data[caucus == "Democrat",
  pres_vote_share := pres_dem_votes / (pres_dem_votes + pres_rep_votes)]
senator_year_data[caucus == "Republican",
  pres_vote_share := pres_rep_votes / (pres_dem_votes + pres_rep_votes)]

# Muriel Humphrey got covariates switched with Hubert, these need corrected
# also, Muriel needs to be dropped
senator_year_data[icpsrLegis == 14516, female := 1]
senator_year_data[icpsrLegis == 14516, drop := 1]
senator_year_data[icpsrLegis == 14516, elected := NA]
senator_year_data[icpsrLegis == 14516, subchr := 0]
senator_year_data[icpsrLegis == 14516, votepct := "58"]
senator_year_data[icpsrLegis == 14516, drop := 1]

senator_year_data[icpsrLegis == 4728 & congress == 95, elected := 1970]
senator_year_data[icpsrLegis == 4728 & congress == 95, votepct := "58"]
senator_year_data[icpsrLegis == 4728 & congress == 95, subchr := 1]
senator_year_data[icpsrLegis == 4728 & congress == 95, female := 0]
senator_year_data[icpsrLegis == 4728 & congress == 95, afam := 0]
senator_year_data[icpsrLegis == 4728 & congress == 95, latino := 0]
senator_year_data[icpsrLegis == 4728 & congress == 95, upperyrs := 0]
senator_year_data[icpsrLegis == 4728 & congress == 95, loweryrs := 0]

# DAN QUAYLE has one observation beyond the time he was in congress
senator_year_data[icpsrLegis == 14447 & congress == 101, drop := 1]

# see who is missing votepct, some are true NAs
# others are not and can be found in our data elsewhere
# correct those we have data for, mark others as drop
votepct_NA <- senator_year_data[is.na(votepct) == TRUE, ]
vt_NA_2 <- senator_year_data[votepct == "", ]
votepct_NA <- rbind(votepct_NA, vt_NA_2)
vt_NA_3 <- senator_year_data[votepct == "N/A"]
votepct_NA <- rbind(votepct_NA, vt_NA_3)
# see if any were removed from check
votepct_NA[icpsrLegis %in% remove_from_check, .(icpsrLegis, congress,
first_name, mc, votepct)]
# select only those who have not been assigned drop
votepct_NA <- votepct_NA[drop == 0,
  .(icpsrLegis, first_name, mc, congress, votepct)]
# correct those we have in the data
# need to decide if we need to drop clinton's "last term"
# she was barely in it
senator_year_data[icpsrLegis == 14512 & congress == 100, votepct := "67"]
senator_year_data[icpsrLegis == 40105 & congress == 111, votepct := "67"] # HILLARY CLINTON
senator_year_data[icpsrLegis == 13050 & congress == 102, votepct := "67"]
senator_year_data[icpsrLegis == 15407 & congress == 104, votepct := "52"]
senator_year_data[icpsrLegis == 14910 & congress == 111, votepct := "53"]
senator_year_data[icpsrLegis == 29537 & congress == 112, votepct := "55"]
senator_year_data[icpsrLegis == 40500 & congress == 111, votepct := "51"]

# some of these will need other values corrected, too
# EDWARD ZORINSKY
senator_year_data[icpsrLegis == 14512 & congress == 100, dem := 1]
senator_year_data[icpsrLegis == 14512 & congress == 100, female := 0]
senator_year_data[icpsrLegis == 14512 & congress == 100, afam := 0]
senator_year_data[icpsrLegis == 14512 & congress == 100, latino := 0]
senator_year_data[icpsrLegis == 14512 & congress == 100, south := 0]
senator_year_data[icpsrLegis == 14512 & congress == 100, south_dem := 0]
senator_year_data[icpsrLegis == 14512 & congress == 100, min_leader := 0]
senator_year_data[icpsrLegis == 14512 & congress == 100, maj_leader := 0]
# HILLARY CLINTON
senator_year_data[icpsrLegis == 40105 & congress == 111, dem := 1]
senator_year_data[icpsrLegis == 40105 & congress == 111, female := 1]
senator_year_data[icpsrLegis == 40105 & congress == 111, afam := 0]
senator_year_data[icpsrLegis == 40105 & congress == 111, latino := 0]
senator_year_data[icpsrLegis == 40105 & congress == 111, south := 0]
senator_year_data[icpsrLegis == 40105 & congress == 111, south_dem := 0]
senator_year_data[icpsrLegis == 40105 & congress == 111, min_leader := 0]
senator_year_data[icpsrLegis == 40105 & congress == 111, maj_leader := 0]
# KEN SALAZAR
senator_year_data[icpsrLegis == 40500, dem := 1]
senator_year_data[icpsrLegis == 40500, south := 0]
senator_year_data[icpsrLegis == 40500, south_dem := 0]
senator_year_data[icpsrLegis == 40500 & congress == 109, latino := 1]
senator_year_data[icpsrLegis == 40500 & congress == 110, latino := 1]
senator_year_data[icpsrLegis == 40500 & congress == 111, latino := 1]

# drop those whose votepct is a real NA or is still missing
senator_year_data[icpsrLegis == 14101 & congress == 111, drop := 1] # JOE BIDEN
senator_year_data[icpsrLegis == 14517 & congress == 95, drop := 1] # MARYON ALLEN
senator_year_data[icpsrLegis == 49103 & congress == 102, drop := 1] # JOCELYN BURDICK
senator_year_data[icpsrLegis == 49905 & congress == 106, drop := 1] # LINCOLN CHAFEE
senator_year_data[icpsrLegis == 20115 & congress == 111, drop := 1] # MARK KIRK
senator_year_data[icpsrLegis == 15633 & congress == 110, drop := 1] # CRAIG THOMAS
senator_year_data[icpsrLegis == 40300 & congress == 108, drop := 1] # LISA MURKOWSKI
senator_year_data[icpsrLegis == 40102 & congress == 107, drop := 1] # JEAN CARNAHAN

# fix JEFFORDS' Congress 107
senator_year_data[icpsrLegis == 94240 & congress == 107, elected := 1988]
senator_year_data[icpsrLegis == 94240 & congress == 107, female := 0]
senator_year_data[icpsrLegis == 94240 & congress == 107, afam := 0]
senator_year_data[icpsrLegis == 94240 & congress == 107, latino := 0]
senator_year_data[icpsrLegis == 94240 & congress == 107, votepct := "50"]
senator_year_data[icpsrLegis == 94240 & congress == 107, state_leg := 1]
senator_year_data[icpsrLegis == 14240 & congress == 107, elected := 1988]
senator_year_data[icpsrLegis == 14240 & congress == 107, female := 0]
senator_year_data[icpsrLegis == 14240 & congress == 107, afam := 0]
senator_year_data[icpsrLegis == 14240 & congress == 107, latino := 0]
senator_year_data[icpsrLegis == 14240 & congress == 107, votepct := "50"]
senator_year_data[icpsrLegis == 14240 & congress == 107, state_leg := 1]

# fix missing data in other areas
senator_year_data[gender == "F", fem := 1]
senator_year_data[gender == "M", fem := 0]

# latino
senator_year_data[is.na(latino) == TRUE, latino := 0]

# BOB MENENDEZ
senator_year_data[icpsrLegis == 29373 & congress == 109, latino := 1]

# JOHN SUNUNU
senator_year_data[icpsrLegis == 29740 & congress == 108, latino := 1]
senator_year_data[icpsrLegis == 29740 & congress == 109, latino := 1]
senator_year_data[icpsrLegis == 29740 & congress == 110, latino := 1]

# afam
senator_year_data[is.na(afam) == TRUE, afam := 0]

# # Fix senators that served discontinuous terms
# # for these, create two mc tags, one for each stint
# # mc will have "XX time in senate" pasted to the end
# # change freshman_congress variable for second term to reflect this
# # also, set class to correct value
# discontinuous_term <- senator_data[grep(";", years_of_service), icpsrLegis]
senator_year_data[icpsrLegis == 2087 & congress <= 93, `:=`(
  mc = paste(mc, "1st time in senate"))]
senator_year_data[icpsrLegis == 2087 & congress >= 94, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 94, class = 3)]
senator_year_data[icpsrLegis == 2822 & congress <= 77, `:=`(
  mc = paste(mc, "1st time in senate"))]
senator_year_data[icpsrLegis == 2822 & congress >= 78, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 78, class = 2)]
senator_year_data[icpsrLegis == 3658 & congress <= 88, `:=`(
  mc = paste(mc, "1st time in senate"))]
senator_year_data[icpsrLegis == 3658 & congress >= 91, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 91, class = 3)]
senator_year_data[icpsrLegis == 4728 & congress <= 88,  `:=`(
  mc = paste(mc, "1st time in senate"))]
senator_year_data[icpsrLegis == 4728 & congress >= 92, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 92, class = 1)]
senator_year_data[icpsrLegis == 14073 & congress <= 93 ,  `:=`(
  mc = paste(mc, "1st time in senate"), class = 3)]
senator_year_data[icpsrLegis == 14073 & congress >= 95, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 95, class = 1)]
senator_year_data[icpsrLegis == 14806 & congress <= 105,  `:=`(
  mc = paste(mc, "1st time in senate"), class = 3)]
senator_year_data[icpsrLegis == 14806 & congress >= 112, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 112, class = 3)]
senator_year_data[icpsrLegis == 14904 & congress <= 100,  `:=`(
  mc = paste(mc, "1st time in senate"), class = 3)]
senator_year_data[icpsrLegis == 14904 & congress >= 101, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 101, class = 1)]
senator_year_data[icpsrLegis == 14914 & congress <= 107,  `:=`(
  mc = paste(mc, "1st time in senate"), class = 1)]
senator_year_data[icpsrLegis == 14914 & congress >= 108, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 108, class = 2)]
senator_year_data[icpsrLegis == 15502 & congress <= 102, `:=`(
  mc = paste(mc, "1st time in senate"), class = 3)]
senator_year_data[icpsrLegis == 15502 & congress >= 103, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 103, class = 1)]

# get first congress in dataset
senator_year_data[, freshman_congress := ceiling(calc_congress(elected + 1))]

# fix first congress for special elections and appointees
fresh_to_check <- senator_year_data[congress < freshman_congress, ]
# fresh_to_check[, .(icpsrLegis, first_name, mc, congress, freshman_congress, drop)]
# senator_year_data[icpsrLegis %in% fresh_to_check$icpsrLegis,
  # unique(freshman_congress), .(icpsrLegis, first_name, mc)]
# these are all off by 1 so subtract 1 from all
senator_year_data[icpsrLegis %in% fresh_to_check$icpsrLegis,
  freshman_congress := freshman_congress - 1]

# fresh_NA <- senator_year_data[is.na(freshman_congress) == TRUE, .(icpsrLegis,
#   first_name, mc, votepct, congress, drop)]
# MARYON ALLEN
senator_year_data[icpsrLegis == 14517, freshman_congress := 95]
# DEAN BARKLEY
senator_year_data[icpsrLegis == 40106, freshman_congress := 107]
# JOE BIDEN
senator_year_data[icpsrLegis == 14101, freshman_congress := 93]
# NICHOLAS BRADY
senator_year_data[icpsrLegis == 14911, freshman_congress := 97]
# JAMES BROYHILL
senator_year_data[icpsrLegis == 10574, freshman_congress := 99]
# HILLARY CLINTON
senator_year_data[icpsrLegis == 40105, freshman_congress := 107]
# CHRIS COONS
senator_year_data[icpsrLegis == 40916, freshman_congress := 111]
# NORRIS COTTON 2nd time in senate
senator_year_data[icpsrLegis == 2087 & congress == 94, freshman_congress == 94]
# SHEILA FRAHM
senator_year_data[icpsrLegis == 49504, freshman_congress := 104]
# CARTE GOODWIN
senator_year_data[icpsrLegis == 40914, freshman_congress := 111]
# PAUL HATFIELD
senator_year_data[icpsrLegis == 14515, freshman_congress := 95]
# KANEASTER HODGES
senator_year_data[icpsrLegis == 14514, freshman_congress := 95]
# MURIEL HUMPHREY
senator_year_data[icpsrLegis == 14516, freshman_congress := 95]
# ROBERT KRUEGER
senator_year_data[icpsrLegis == 14247, freshman_congress := 103]
# GEORGE LEMIEUX
senator_year_data[icpsrLegis == 40911, freshman_congress := 111]
# HOWARD METZENBAUM 1st time in senate
senator_year_data[icpsrLegis == 14073 & congress == 93, freshman_congress := 93]
# DAN QUAYLE
senator_year_data[icpsrLegis == 14447, freshman_congress := 97]
# KEN SALAZAR
senator_year_data[icpsrLegis == 40500, freshman_congress := 109]
# BRIAN SCHATZ
senator_year_data[icpsrLegis == 41112, freshman_congress := 112]

# Calculate freshman
senator_year_data[, freshman := 0]
senator_year_data[congress == freshman_congress |
  congress == freshman_congress + 1 | congress == freshman_congress + 2,
  freshman := 1]

# Make superfreshman
senator_year_data[, superfreshman := 0]
senator_year_data[mc %like% "2nd time in senate" & freshman == 1,
  superfreshman := 1]

# Calculate retiree
senator_year_data[, retiree := 0]
senator_year_data[, last_congress := max(congress), icpsrLegis]
senator_year_data[congress == last_congress &
  did_not_seek_reelection == 1, retiree := 1]

# Calculate seniority
senator_year_data[, seniority := 1 + congress - freshman_congress]
# add seniority variable by senate term-length
senator_year_data[, senate_seniority := ceiling(seniority / 3)]

# # check votepct stability over senate term
# # these should be the same for legislators except in cases of special elections
# senator_year_data[, votepct := as.numeric(votepct)]
# votepct_term1 <- senator_year_data[senate_seniority == 1, .(icpsrLegis, first_name, mc,
#   congress, votepct)]
# votepct_term1[, mean_votepct := mean(votepct, na.rm = TRUE), .(icpsrLegis, mc)]
# term1_votepct_check <- votepct_term1[votepct != mean_votepct, ]
# votepct_term2 <- senator_year_data[senate_seniority == 2, .(icpsrLegis, first_name, mc,
#   congress, votepct)]
# votepct_term2[, mean_votepct := mean(votepct, na.rm = TRUE), .(icpsrLegis, mc)]
# term2_votepct_check <- votepct_term2[votepct != mean_votepct, ]
# # many errors, ask William and Craig if we want to fix these
# # alternative is use 2 party vote share

# # check party coding
# senator_year_data[party == "D" & dem != 1, ]
# senator_year_data[party == "R" & dem === 1, ]

# # figure out who isn't coded as dem == 1 | dem == 0
# unique(senator_year_data$dem)
# senator_year_data[is.na(dem) == TRUE, ]
# senator_year_data[dem == 112, ]
# senator_year_data[dem == 328, ]

# correct party for sen. metzenbaum
senator_year_data[icpsrLegis == 14073, dem := 1]

# create caucus majority variable
# senator_year_data[dem == 1 & majority == 1, unique(congress)]
dem_majority <- c(93:96, 100:103, 110:112)
rep_majority <- c(97:99, 104:106, 108:109)
senator_year_data[caucus == "Democrat" & congress %in% dem_majority,
  caucus_majority := 1]
senator_year_data[caucus == "Democrat" & congress %in% rep_majority,
  caucus_majority := 0]
senator_year_data[caucus == "Republican" & congress %in% rep_majority,
  caucus_majority := 1]
senator_year_data[caucus == "Republican" & congress %in% dem_majority,
  caucus_majority := 0]

# add caucus majority for Congress 107
senator_year_data[caucus == "Democrat" & congress == 107,
  caucus_majority := 0.75]
senator_year_data[caucus == "Republican" & congress == 107,
  caucus_majority := 0.25]
senator_year_data[congress == 107 & mc %like% "JEFFORDS", caucus_majority := 1]

# Populate committee chair
committees_93102 <- read.delim("inst/extdata/senate_assignments_80-102.txt",
  header = FALSE, sep = "\n")
committees_103112 <- gdata::read.xls("inst/extdata/senate_assignments_103-11.xls")
setDT(committees_93102)
setDT(committees_103112)
committees_93102[, icpsrLegis := as.numeric(substr(V1, 15, 19))]
committees_93102[, congress := as.numeric(substr(V1, 48, 50))]
committees_93102[, seniorParty := as.numeric(substr(V1, 57, 58))]
committees_93102 <- committees_93102[seniorParty %in% 11:19,
  .(congress, icpsrLegis)]
committees_103112 <- committees_103112[
  Senior.Party.Member %in% c(11, 12, 13, 14, 16),
  .(Congress, ID..)]
setnames(committees_103112, c("congress", "icpsrLegis"))
committees <- rbind(committees_93102, committees_103112)
chair_dt <- committees[, .N, .(congress, icpsrLegis)][, .(congress, icpsrLegis)]
chair_dt[, com_chair := 1]
senator_year_data <- merge(senator_year_data, chair_dt,
  by = c("congress", "icpsrLegis"), all.x = TRUE)
senator_year_data[is.na(com_chair) == TRUE, com_chair := 0]

# # check chair vs com_chair
# senator_year_data[chair == 1 & com_chair == 0, .(congress, icpsrLegis,
#   first_name, mc, caucus, caucus_majority)]
# senator_year_data[chair == 0 & com_chair == 1, .(congress, icpsrLegis,
#   first_name, mc, caucus, caucus_majority)]
# senator_year_data[com_chair == 1 & caucus_majority == 0, .(congress, icpsrLegis,
#   first_name, mc, caucus)]

# Build committee data and merge in
source("package/get-best-committee.R")
senator_year_data <- merge(senator_year_data, best_committee_dt,
  by = c("congress", "icpsrLegis"), all.x = TRUE)

# Add indicator for Gingrich senators
gingrich_senators <- fread("inst/extdata/gingrich_senators.csv")
senator_data[, gingrich_senator := 0]
senator_data[icpsrLegis %in% gingrich_senators[, icpsrLegis],
  gingrich_senator := 1]
senator_year_data[, gingrich_senator := 0]
senator_year_data[icpsrLegis %in% gingrich_senators[, icpsrLegis],
  gingrich_senator := 1]

# add leader variable
senator_year_data[, leader := 0]
senator_year_data[maj_leader == 1 | min_leader == 1, leader := 1]

# Populate up_for_reelction
senator_year_data[, up_for_reelection := 0]
senator_year_data[(class == 1 & congress %in% seq(1, 120, 3)) |
    (class == 2 & congress %in% seq(2, 120, 3)) |
    (class == 3 & congress %in% seq(3, 120, 3)),
  up_for_reelection := 1]

# drop unused variables
senator_year_data <- senator_year_data[, .(
  congress, icpsrLegis, stabb, class, first_name, mc, caucus, caucus_majority,
  pres_vote_share, vote_share, south, south11, south13, south17, south_dem,
  leader, com_chair, best_committee, power_committee, up_for_reelection,
  freshman, superfreshman, seniority, senate_seniority, retiree, afam, fem,
  latino, gingrich_senator, drop)]

setnames(senator_year_data, "fem", "female")
setnames(senator_year_data, "com_chair", "chair")

save(senator_year_data, file = "test_data/senator_year_data.RData")
