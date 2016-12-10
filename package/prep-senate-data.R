# 1. load packages and all external data
options(stringsAsFactors = FALSE)
library(partycalls)
library(yaml)
states <- fread("inst/extdata/states.csv")
states[, fips := sprintf("%02.f", fips)]
load("inst/extdata/senate93-112.RData")

# 2. make data for package
# Code party calls; make responsiveness data
set.seed(201375487)
senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate")
names(senate_party_calls) <- paste0("sen", 93:112)
save(senate_party_calls, file = "inst/extdata/senate_party_calls.Rdata")
load("inst/extdata/senate_party_calls.Rdata")

# Generate senator year data (i.e., data for analysis)
syd_list <- lapply(93:112, make_senator_year_data, senate_party_calls)
senator_year_data <- rbindlist(lapply(syd_list, function(x) x$senator_year_data))
setnames(senator_year_data, "state", "stabb")

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
senator_data <- senator_year_data[, .N, .(icpsrLegis, mc)]

# Populate stabb
senator_data[, stabb := substr(as.character(mc), nchar(as.character(mc)) - 2,
  nchar(as.character(mc)) - 1)]
# Clean stabb's when icpsrLegis has different format
senator_data[mc == "ALLEN (D AL) 14517", stabb := "AL"]
senator_data[mc == "HUMPHREY (D MN) 14516", stabb := "MN"]
senator_data[mc == "CHAFEE (R RI) 49905", stabb := "RI"]
senator_data[mc == "PRYOR (D AR) 10791", stabb := "AR"]
senator_data[mc == "PRYOR (D AR) 40301", stabb := "AR"]
senator_data[mc == "BENNETT (R UT) 645", stabb := "UT"]
senator_data[mc == "MURKOWSKI (R AK) 14907", stabb := "AK"]
senator_data[mc == "MURKOWSKI (R AK) 40300", stabb := "AK"]
senator_data[mc == "BENNETT (R UT) 49307", stabb := "UT"]
senator_data[mc == "BAYH (D IN) 10800", stabb := "IN"]
senator_data[mc == "BAYH (D IN) 49901", stabb := "IN"]

# Populate south indicators
south11 <- c("SC", "MS", "FL", "AL", "AR", "GA", "LA", "TX", "VA", "TN", "NC")
south13 <- c(south11, "OK", "KY")
south17 <- c(south13, "DE", "WV", "MD", "MO")
senator_data[, south11 := 1 * (stabb %in% south11)]
senator_data[, south13 := 1 * (stabb %in% south13)]
senator_data[, south17 := 1 * (stabb %in% south17)]

# Merge in govtrack covarates
source("package/get_govtrack_legislators_csv.R")
senator_data <- merge(senator_data, legislators, by = "icpsrLegis")

# Populate class
source("package/get_govtrack_legislators_yaml_data.R")
senator_data <- merge(senator_data, legislators_yaml, by = "icpsrLegis")

# Populate afam
afr_am_dt <- data.table(
  "mc" = c("BROOKE (R MA)", "MOSELEY-BRA (D IL)", "OBAMA (D IL)",
    "OBAMA (D USA)", "BURRIS (D IL)"),
  "icpsrLegis" = c(11201, 49303, 40502, 99911, 40903))
senator_data[, afam := ifelse(icpsrLegis %in% afr_am_dt$icpsrLegis, 1, 0)]

# Populate female
female_dt <- data.table(
  "mc" = c("ALLEN (D AL) 14517", "HUMPHREY (D MN) 14516", "KASSEBAUM (R KS)",
    "MIKULSKI (D MD)", "BURDICK2 (D ND)", "BOXER (D CA)", "FEINSTEIN (D CA)",
    "MOSELEY-BRA (D IL)", "HUTCHISON (R TX)", "MURRAY (D WA)", "FRAHM (R KS)",
    "SNOWE (R ME)", "LANDRIEU (D LA)", "COLLINS (R ME)", "LINCOLN (D AR)",
    "STABENOW (D MI)", "CLINTON (D NY)", "CANTWELL (D WA)", "MURKOWSKI (R AK)",
    "DOLE (R NC)", "KLOBUCHAR (D MN)", "SHAHEEN (D NH)", "GILLIBRAND (D NY)",
    "AYOTTE (R NH)"),
  "icpsrLegis" = c(14517, 14516, 14708, 14440, 49103, 15011, 49300,
    49303, 49306, 49308, 49504, 14661, 49702, 49703, 29305, 29732,
    40105, 39310, 40300, 40303, 40700, 40906, 20735, 41106))
senator_data[, female := ifelse(icpsrLegis %in% female_dt$icpsrLegis, 1, 0)]

# Populate latino
latino_dt <- data.table(
  "mc" = c("MONTOYA (D NM)", "SUNUNU (R NH)", "SALAZAR (D CO)",
    "MARTINEZ (R FL)", "MENENDEZ (D NJ)", "RUBIO (R FL)"),
  "icpsrLegis" = c(6611, 29740, 40500, 40501, 29373, 41102))
senator_data[, latino := ifelse(icpsrLegis %in% latino_dt$icpsrLegis, 1, 0)]

# Populate years of service
source("package/get_bios.R")
senator_data[, years_of_service := substr(bios,
  regexpr("Senate Years of Service", bios) + 25, regexpr("Party", bios) - 2)]
senator_data[, years_of_service := gsub(",", ";", years_of_service)]
senator_data[icpsrLegis == 9369, years_of_service := "1954-2003"]
senator_data[icpsrLegis == 10802, years_of_service := "1965-1983"]

# Fix senators that served discontinuous terms
# for these, create two icpsrLegis & mc tags, one for each stint
# to do so, break up senator_data into three pieces:
# _1 is the ones that are already fine
# _2 is the first stints
# _3 is the second stints
# fix _2 and _3, meaning fix years of service, and
# change mc & icpsrLegis to reflect stint
# mc will have "XX time in senate" pasted to the end
# icpsrLegis will have 100000 or 200000 added
# rbind together and replace senator_data
# finally, change icpsrLegis for the related lines of senator_year_data
icpsrLegis_to_change <- senator_data[grep(";", years_of_service), icpsrLegis]
senator_data_1 <- senator_data[!icpsrLegis %in% icpsrLegis_to_change]
senator_data_2 <- senator_data[icpsrLegis %in% icpsrLegis_to_change]
senator_data_3 <- copy(senator_data_2)
senator_data_2[, mc := paste(mc, "1st time in senate")]
senator_data_2[,
  years_of_service := substring(years_of_service, 1,
    regexpr(";", years_of_service) - 1)]
senator_data_2[icpsrLegis == 2087, class := 3]
senator_data_2 <- senator_data_2[icpsrLegis != 2822] # drop stint before 1973
senator_data_2 <- senator_data_2[icpsrLegis != 3658] # drop stint before 1973
senator_data_2 <- senator_data_2[icpsrLegis != 4728] # drop stint before 1973
senator_data_2[icpsrLegis == 14073, class := 3]
senator_data_2[icpsrLegis == 14806, class := 3]
senator_data_2[icpsrLegis == 14904, class := 3]
senator_data_2[icpsrLegis == 14914, class := 1]
senator_data_2[icpsrLegis == 15502, class := 3]
senator_data_3[, mc := paste(mc, "2nd time in senate")]
senator_data_3[,
  years_of_service := substring(years_of_service,
    regexpr(";", years_of_service) + 2, 100)]
senator_data_3[icpsrLegis == 2087, class := 3]
senator_data_3[icpsrLegis == 2822, class := 2]
senator_data_3[icpsrLegis == 3658, class := 3]
senator_data_3[icpsrLegis == 4728, class := 1]
senator_data_3[icpsrLegis == 14073, class := 1]
senator_data_3[icpsrLegis == 14806, class := 3]
senator_data_3[icpsrLegis == 14904, class := 1]
senator_data_3[icpsrLegis == 14914, class := 2]
senator_data_3[icpsrLegis == 15502, class := 1]
senator_data_2[, icpsrLegis := 100000 + icpsrLegis]
senator_data_3[, icpsrLegis := 200000 + icpsrLegis]
senator_data <- rbind(senator_data_1, senator_data_2, senator_data_3)
senator_year_data[icpsrLegis == 2087 & congress <= 93, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 2087 & congress >= 94, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 2822 & congress <= 77, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 2822 & congress >= 78, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 3658 & congress <= 88, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 3658 & congress >= 91, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 4728 & congress <= 88,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 4728 & congress >= 92, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14073 & congress <= 93 ,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14073 & congress >= 95, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14806 & congress <= 105,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14806 & congress >= 112, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14904 & congress <= 100,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14904 & congress >= 101, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14914 & congress <= 107,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14914 & congress >= 108, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 15502 & congress <= 102, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 15502 & congress >= 103, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]

# Populate died_in_office, defeated_for_renomination, defeated_for_reelection,
#   changed_party_affiliation, resigned, did_not_seek_reelection
source("package/mine_bios.R")

# Populate freshman
freshman_dt <- fread("inst/extdata/freshman_dt.csv")
freshman_dt <- freshman_dt[, .(congress, icpsrLegis)]
setnames(freshman_dt, "congress", "freshman_congress")
senator_data <- merge(senator_data, freshman_dt, by = "icpsrLegis")

## END TIME-INVARIANT COVARIATES

## BEGIN TIME-VARYING COVARIATES

# Merge TIME-INVARIANT COVARIATES into senator_year_data
senator_year_data <- merge(senator_year_data, senator_data,
  by = c("icpsrLegis", "mc", "stabb"), all.x = TRUE)

# Populate party leader
leader_dt <- fread("inst/extdata/leader_DT.csv")
leader_dt <- merge(leader_dt,
  senator_year_data[, .N, .(mc, icpsrLegis)][, .(mc, icpsrLegis)],
  by = "mc")
leader_dt <- leader_dt[, .N, .(congress, icpsrLegis)][, .(congress, icpsrLegis)]
leader_dt[, leader := 1]
senator_year_data <- merge(senator_year_data, leader_dt,
  by = c("congress", "icpsrLegis"), all.x = TRUE)
senator_year_data[is.na(leader), leader := 0]

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
chair_dt[, chair := 1]
senator_year_data <- merge(senator_year_data, chair_dt,
  by = c("congress", "icpsrLegis"), all.x = TRUE)
senator_year_data[is.na(chair), chair := 0]

# Populate up_for_reelction
senator_year_data[, up_for_reelection := 0]
senator_year_data[(class == 1 & congress %in% seq(1, 120, 3)) |
    (class == 2 & congress %in% seq(2, 120, 3)) |
    (class == 3 & congress %in% seq(3, 120, 3)),
  up_for_reelection := 1]
# TODO(minozzi): add in appointees up for special election?

# Populate year of most recent normal (i.e., on-cycle) election
senator_year_data[,
  year_of_most_recent_normal_election := calc_year_elected(congress, class)]

# Populate on-cycle elections data
source("package/senate_seat_elections.R")
senator_year_data <- merge(senator_year_data, senate_seat_elections,
  by.x = c("year_of_most_recent_normal_election", "stabb", "class"),
  by.y = c("election_year", "stabb", "class"))

# Populate party caucus
senator_year_data[party == "D", caucus := "Democrat"]
senator_year_data[party == "R", caucus := "Republican"]
senator_year_data[party == "D", caucus := "Democrat"]
senator_year_data[party == "Conservative", caucus := "Republican"]
senator_year_data[icpsrLegis == 94240, caucus := "Democrat"]
senator_year_data[icpsrLegis == 29147, caucus := "Democrat"]
senator_year_data[mc == "BARKLEY (Indep MN)", caucus := "Republican"]
senator_year_data[mc == "BYRD (Indep VA)", caucus := "Democrat"]

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

# For senators appointed to fill remaining terms, replace election returns
# with data from special elections when available
source("package/fix_special_elections.R")

# here are the times when we use election results from a different
# candidate's previous victory
# Mark these as "drop"
senator_year_data[icpsrLegis == "29373", last_name := "Menendez"]
check <- sapply(1:2040, function(i) {
  !grepl(senator_year_data$last_name[i],
    senator_year_data$candname_vote_rank_1[i], ignore.case = TRUE)
})
check <- senator_year_data[check, .(congress, stabb, icpsrLegis,
  last_name, candname_vote_rank_1)]
senator_year_data[, drop := 0]
for (i in 1:nrow(check)) {
  iL <- check$icpsrLegis[i]
  cong <- check$congress[i]
  senator_year_data[icpsrLegis == iL & congress == cong,
    drop := 1]
}

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

# Calculate freshman
senator_year_data[, freshman := 0]
senator_year_data[congress == freshman_congress |
    congress == freshman_congress + 1 | congress == freshman_congress + 2,
  freshman := 1]

# Calculate retiree
senator_year_data[, retiree := 0]
senator_year_data[, last_congress := max(congress), icpsrLegis]
senator_year_data[congress == last_congress &
    did_not_seek_reelection == 1, retiree := 1]

# Calculate seniority
senator_year_data[, seniority := congress - freshman_congress]

# Make superfreshman
senator_year_data[, superfreshman := 0]
senator_year_data[mc %like% "2nd time in senate", superfreshman := 1]

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

## END TIME-VARYING COVARIATES
setnames(senator_year_data, "pf_ideal", "party_free_ideal_point")
senator_year_data <- senator_year_data[, .(
  congress, icpsrLegis, stabb, class, first_name, last_name, caucus,
  responsiveness_party_calls, responsiveness_noncalls, party_free_ideal_point,
  dist_from_floor_median, dist_from_party_median,
  ideological_extremism, pres_vote_share, vote_share,
  leader, chair, best_committee, power_committee, up_for_reelection, freshman,
  superfreshman, seniority, retiree, south11, south13, south17, afam, female,
  latino, gingrich_senator, drop)]

# 4. add datasets to package, build, install
devtools::use_data(senate_party_calls, senator_year_data,
  senator_data,
  overwrite = TRUE)
devtools::build()
devtools::install()
