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

# Merge in govtrack covariates
source("package/get_govtrack_legislators_csv.R")
senator_data <- merge(senator_data, legislators, by = "icpsrLegis",
  all.x = TRUE)


###############################################
# FIGURE OUT WHO ISN'T GETTING GOVTRACK STUFF #
###############################################

# Populate class
source("package/get_govtrack_legislators_yaml_data.R")
senator_data <- merge(senator_data, legislators_yaml, by = "icpsrLegis",
  all.x = TRUE)


##############################################
# FIGURE OUT IF ANYONE ISN'T GETTING A CLASS #
##############################################





# Populate years of service
source("package/get_bios.R")
senator_data[, years_of_service := substr(bios,
  regexpr("Senate Years of Service", bios) + 25, regexpr("Party", bios) - 2)]
senator_data[, years_of_service := gsub(",", ";", years_of_service)]
senator_data[icpsrLegis == 9369, years_of_service := "1954-2003"]
senator_data[icpsrLegis == 10802, years_of_service := "1965-1983"]

# Populate died_in_office, defeated_for_renomination, defeated_for_reelection,
#   changed_party_affiliation, resigned, did_not_seek_reelection
source("package/mine_bios.R")

# # Populate different south indicators
# south11 <- c("SC", "MS", "FL", "AL", "AR", "GA", "LA", "TX", "VA", "TN", "NC")
# south13 <- c(south11, "OK", "KY")
# south17 <- c(south13, "DE", "WV", "MD", "MO")
# senator_data[, south11 := 1 * (stabb %in% south11)]
# senator_data[, south13 := 1 * (stabb %in% south13)]
# senator_data[, south17 := 1 * (stabb %in% south17)]

# # END TIME INVARIANT COVARIATES

# Merge TIME-INVARIANT COVARIATES into senator_year_data
senator_year_data <- merge(senator_year_data, senator_data,
    by = c("icpsrLegis", "mc", "stabb"), all.x = TRUE)

# create south indicators
south11 <- c("SC", "MS", "FL", "AL", "AR", "GA", "LA", "TX", "VA", "TN", "NC")
south13 <- c(south11, "OK", "KY")
south17 <- c(south13, "DE", "WV", "MD", "MO")
senator_year_data[, south11 := 1 * (stabb %in% south11)]
senator_year_data[, south13 := 1 * (stabb %in% south13)]
senator_year_data[, south17 := 1 * (stabb %in% south17)]

# # BEGIN TIME VARYING COVARIATES

# load les data and make into a data.table
les_senate <- fread("inst/extdata/93_113_senate_variables.csv")
setDT(les_senate)
# remove unused variables/congresses from les data
les_senate <- les_senate[congress != 113, ]
les_senate[, c("thomas_num", "thomas_name", "st_name", "votepct_sq",
  "meddist", "majdist", "speaker", "power", "budget", "state_leg_prof",
  "ss_bills", "ss_aic", "ss_abc", "ss_pass", "ss_law", "s_bills", "s_aic",
  "s_abc", "s_pass", "s_law", "c_bills", "c_aic", "c_abc", "c_pass", "c_law",
  "all_bills", "all_aic", "all_abc", "all_pass", "all_law", "les",
  "leslag", "seniority", "sensq") := NULL]

# # see if anyone is missing
# les_icpsr <- les_senate$icpsr
# senator_year_data[, in_les_data := 1 * (icpsrLegis %in% les_icpsr)]
# senator_year_data[in_les_data == 0, ]

# merge in les variables
senator_year_data <- merge(senator_year_data, les_senate,
  by.x = c("icpsrLegis", "congress"), by.y = c("icpsr", "congress"),
  all.x = TRUE)

# # check year elected variable
# freshman_dt <- senator_year_data[, .(
#   elected_mean = mean(elected),
#   elected_mean_no_NA = mean(elected, na.rm = TRUE)
# ), .(icpsrLegis)]
# senator_year_data <- merge(senator_year_data, freshman_dt, by = "icpsrLegis")
# freshman_congress_check <-
#   senator_year_data[elected != elected_mean, ]
# freshman_congress_check2 <-
#   senator_year_data[elected != elected_mean_no_NA]
# unique(freshman_congress_check$icpsrLegis)

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

# check JEFFORDS
# jeffords_DT <- senator_year_data[mc %like% "JEFFORDS", ]

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


# see who is missing class data
class_NA <- senator_year_data[is.na(class) == TRUE]

# correct missingness
senator_year_data[, south11 := ]



# get first congress in dataset
senator_year_data[, freshman_congress := ceiling(calc_congress(elected + 1))]

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
  mc = paste(mc, "2nd time in senate"), freshman_congress, class = 2)]
senator_year_data[icpsrLegis == 15502 & congress <= 102, `:=`(
  mc = paste(mc, "1st time in senate"), class = 3)]
senator_year_data[icpsrLegis == 15502 & congress >= 103, `:=`(
  mc = paste(mc, "2nd time in senate"), freshman_congress = 103, class = 1)]

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
senator_year_data[, seniority := congress - freshman_congress]
# add seniority variable by senate term-length
senator_year_data[, senate_seniority := floor(seniority / 3)]

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

# create party caucus variable
senator_year_data[dem == 1, caucus := "Democrat"]
senator_year_data[dem == 0, caucus := "Republican"]
senator_year_data[dem == 112, caucus := "Republican"]
senator_year_data[dem == 328, caucus := "Democrat"]


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

# Populate year of most recent normal (i.e., on-cycle) election
senator_year_data[,
  year_of_most_recent_normal_election := calc_year_elected(congress, class)]

# # Populate on-cycle elections data
# source("package/senate_seat_elections.R")
# senator_year_data <- merge(senator_year_data, senate_seat_elections,
#   by.x = c("year_of_most_recent_normal_election", "stabb", "class"),
#   by.y = c("election_year", "stabb", "class"))

# # For senators appointed to fill remaining terms, replace election returns
# # with data from special elections when available
# source("package/fix_special_elections.R")

# add in presidential election data
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

# # Populate committee chair
# committees_93102 <- read.delim("inst/extdata/senate_assignments_80-102.txt",
#   header = FALSE, sep = "\n")
# committees_103112 <- gdata::read.xls("inst/extdata/senate_assignments_103-11.xls")
# setDT(committees_93102)
# setDT(committees_103112)
# committees_93102[, icpsrLegis := as.numeric(substr(V1, 15, 19))]
# committees_93102[, congress := as.numeric(substr(V1, 48, 50))]
# committees_93102[, seniorParty := as.numeric(substr(V1, 57, 58))]
# committees_93102 <- committees_93102[seniorParty %in% 11:19,
#   .(congress, icpsrLegis)]
# committees_103112 <- committees_103112[
#   Senior.Party.Member %in% c(11, 12, 13, 14, 16),
#   .(Congress, ID..)]
# setnames(committees_103112, c("congress", "icpsrLegis"))
# committees <- rbind(committees_93102, committees_103112)
# chair_dt <- committees[, .N, .(congress, icpsrLegis)][, .(congress, icpsrLegis)]
# chair_dt[, com_chair := 1]
# senator_year_data <- merge(senator_year_data, chair_dt,
#   by = c("congress", "icpsrLegis"), all.x = TRUE)
# senator_year_data[is.na(chair), com_chair := 0]
#
# # check chair vs com_chair
# senator_year_data[chair == 1 & com_chair == 0, ]
# senator_year_data[chair == 0 & com_chair == 1, ]

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

# make votepct numeric
senator_year_data[, votepct := as.numeric(votepct)]

# drop unused variables
senator_year_data <- senator_year_data[, .(
  congress, icpsrLegis, stabb, class, first_name, last_name, caucus,
  pres_vote_share, pres_dem_votes, votepct, south, south11, south13, south17,
  leader, chair, best_committee, power_committee, up_for_reelection, freshman,
  superfreshman, seniority, retiree, afam, female, latino, gingrich_senator,
  caucus_majority)]

save(senator_year_data, file = "test_data/senator_year_data.RData")
