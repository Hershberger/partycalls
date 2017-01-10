library(partycalls)
# library(data.table)
library(yaml)
options(stringsAsFactors = FALSE)
states <- fread("inst/extdata/states.csv")
states[, fips := sprintf("%02.f", fips)]

# load party calls data
load("test_data/senate_party_calls_emIRT_only.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

senator_year_data <- rbindlist(lapply(93:112, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, senate_party_calls, chamber = "senate")
  DATA <- rc$member_year_data
  DATA[, .(congress,
    icpsrLegis,
    state, mc, party,
    party_free_ideal_point = pf_ideal,
    pirate100 = 100 * responsiveness_party_calls,
    pfrate100 = 100 * responsiveness_noncalls,
    ideological_extremism)]
}))
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

# to see who is lost
senator_year_data2 <- senator_year_data

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
    "AYOTTE (R NH)", "HAWKINS (R FL)", "CARNAHAN (D MO)", "MCCASKILL (D MO)",
    "HAGAN (D NC)"),
  "icpsrLegis" = c(14517, 14516, 14708, 14440, 49103, 15011, 49300, 49303,
    49306, 49308, 49504, 14661, 49702, 49703, 29305, 29732, 40105, 39310, 40300,
    40303, 40700, 40906, 20735, 41106, 14905, 40102, 40701, 40907))
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
source("package/fix_special_elections_old.R")

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

les_data <- read.csv("inst/extdata/93_113_senate_variables.csv")
setDT(les_data)
les_variables <- data.table(
  icpsrLegis = les_data$icpsr,
  # year = les_data$year,
  congress = les_data$congress,
  # female = les_data$female, afam = les_data$afam,
  # latino = les_data$latino,
  # dwnom1 = les_data$dwnom1,
  # chair = les_data$chair, majority = les_data$majority,
  # subchair = les_data$subchr, state_leg = les_data$state_leg,
  # maj_leader = les_data$maj_leader, min_leader = les_data$min_leader,
  votepct = les_data$votepct)
# les_variables[, leader := 0]
# les_variables[maj_leader == 1, leader := 1]
# les_variables[min_leader == 1, leader := 1]
# les_variables$majority <- as.numeric(les_data$majority)
senator_year_data <- merge(senator_year_data, les_variables,
  by = c("congress", "icpsrLegis"))

# # fix seniority
# # use this if using les_data seniority
# senator_year_data[seniority == "2.5?", seniority := 2]
# senator_year_data$seniority <- as.numeric(senator_year_data$seniority)

# majority party if not using les data
senator_year_data[, majority := 0]
senator_year_data[caucus == "Democrat" & congress %in% c(93:96, 100:103, 110:112) |
    caucus == "Republican" & congress %in% c(97:99, 104:106),
  majority := 1]
# senate 107 majority flips multiple times
# probably best not to include it in analyses that involve majority party stuff
senator_year_data[congress == 107, majority := NA]

## END TIME-VARYING COVARIATES
# setnames(senator_year_data, "pf_ideal", "party_free_ideal_point")
senator_year_data <- senator_year_data[, .(
  congress, icpsrLegis, stabb, class, first_name, last_name, caucus, majority,
  pfrate100, pirate100, party_free_ideal_point,
  ideological_extremism,
  pres_vote_share, vote_share, votepct, best_committee,
  # maj_leader, min_leader, subchair, state_leg,
  leader, chair, power_committee, up_for_reelection, freshman,
  superfreshman, seniority, retiree, south11, south13, south17, afam, female,
  latino, gingrich_senator, drop)]


get_vote_counts_by_congress <- function(congress) {
  rc <- get(paste0("sen", congress))
  ld <- rc$legis.data
  ld$mc <- rownames(ld)
  setDT(ld)
  vt <- rc$votes
  vt[vt %in% 1:6] <- 1
  vt[vt == 0 | vt %in% 7:9] <- 0
  vt <- data.table(mc = rownames(vt), votes = rowSums(vt), congress = congress)
  ld <- merge(ld, vt, by = "mc")
  setnames(ld, "state", "stabb")
  ld[stabb != "USA", .(mc, congress, stabb, icpsrState, icpsrLegis, party,
    partyCode, votes)]
}

vte_list <- lapply(93:112, get_vote_counts_by_congress)
vte_data <- rbindlist(vte_list)
setDT(vte_data)

sen_lep <- read.csv("inst/extdata/93_113_senate_variables.csv")
setDT(sen_lep)

# search for who is dropped in 93
syd93_1 <- senator_year_data[congress == 93, ]
syd93_2 <- senator_year_data2[congress == 93, ]
syd93_2[, not_dropped := 1 * (icpsrLegis %in% syd93_1$icpsrLegis), ]
syd93_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen93 obs elected
sen_lep[thomas_name %like% "COTTON" & congress == 93, votepct]
sen_lep[thomas_name %like% "EASTLAND" & congress == 93, votepct]
sen_lep[thomas_name %like% "GOLDWATER" & congress == 93, votepct]
sen_lep[thomas_name %like% "HUMPHREY" & congress == 93, votepct]
sen_lep[thomas_name %like% "LAXALT" & congress == 93, votepct] # NA
sen_lep[thomas_name %like% "METZENBAUM" & congress == 93, votepct] # NA

# how many votes by missing sen93 obs
vte_data[mc %like% "COTTON" & congress == 93, votes]
vte_data[mc %like% "EASTLAND" & congress == 93, votes]
vte_data[mc %like% "GOLDWATER" & congress == 93, votes]
vte_data[mc %like% "HUMPHREY" & congress == 93, votes]
vte_data[mc %like% "LAXALT" & congress == 93, votes] # 8
vte_data[mc %like% "METZENBAUM" & congress == 93, votes]


# search for who is dropped in 94
syd94_1 <- senator_year_data[congress == 94, ]
syd94_2 <- senator_year_data2[congress == 94, ]
syd94_2[, not_dropped := 1 * (icpsrLegis %in% syd94_1$icpsrLegis), ]
syd94_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen94 obs elected
sen_lep[thomas_name %like% "COTTON" & congress == 94, votepct] # NA
sen_lep[thomas_name %like% "EASTLAND" & congress == 94, votepct]
sen_lep[thomas_name %like% "GOLDWATER" & congress == 94, votepct]
sen_lep[thomas_name %like% "HUMPHREY" & congress == 94, votepct]

# how many votes by missing sen94 obs
vte_data[mc %like% "COTTON" & congress == 94, votes] # 17
vte_data[mc %like% "EASTLAND" & congress == 94, votes]
vte_data[mc %like% "GOLDWATER" & congress == 94, votes]
vte_data[mc %like% "HUMPHREY" & congress == 94, votes]


# search for who is dropped in 95
syd95_1 <- senator_year_data[congress == 95, ]
syd95_2 <- senator_year_data2[congress == 95, ]
syd95_2[, not_dropped := 1 * (icpsrLegis %in% syd95_1$icpsrLegis), ]
syd95_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen95 obs elected
sen_lep[thomas_name %like% "COTTON" & congress == 95, votepct] # NA
sen_lep[thomas_name %like% "EASTLAND" & congress == 95, votepct]
sen_lep[thomas_name %like% "GOLDWATER" & congress == 95, votepct]
sen_lep[thomas_name %like% "HUMPHREY" & congress == 95, votepct] # TWO OBS

# how many votes by missing sen95 obs
vte_data[mc %like% "COTTON" & congress == 95, votes] # NA
vte_data[mc %like% "EASTLAND" & congress == 95, votes]
vte_data[mc %like% "GOLDWATER" & congress == 95, votes]
vte_data[mc %like% "HUMPHREY" & congress == 95, votes] # TWO  OBS


# search for who is dropped in 96
syd96_1 <- senator_year_data[congress == 96, ]
syd96_2 <- senator_year_data2[congress == 96, ]
syd96_2[, not_dropped := 1 * (icpsrLegis %in% syd96_1$icpsrLegis), ]
syd96_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen96 obs elected
sen_lep[thomas_name %like% "GOLDWATER" & congress == 96, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 96, votepct]

# how many votes by missing sen96 obs
vte_data[mc %like% "GOLDWATER" & congress == 96, votes]
vte_data[mc %like% "METZENBAUM" & congress == 96, votes]


# search for who is dropped in 97
syd97_1 <- senator_year_data[congress == 97, ]
syd97_2 <- senator_year_data2[congress == 97, ]
syd97_2[, not_dropped := 1 * (icpsrLegis %in% syd97_1$icpsrLegis), ]
syd97_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen97 obs elected
sen_lep[thomas_name %like% "GOLDWATER" & congress == 97, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 97, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 97, votepct]

# how many votes by missing sen97 obs
vte_data[mc %like% "GOLDWATER" & congress == 97, votes]
vte_data[mc %like% "METZENBAUM" & congress == 97, votes]
vte_data[mc %like% "GORTON" & congress == 97, votes]


# search for who is dropped in 98
syd98_1 <- senator_year_data[congress == 98, ]
syd98_2 <- senator_year_data2[congress == 98, ]
syd98_2[, not_dropped := 1 * (icpsrLegis %in% syd98_1$icpsrLegis), ]
syd98_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen98 obs elected
sen_lep[thomas_name %like% "GOLDWATER" & congress == 98, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 98, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 98, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 98, votepct]

# how many votes by missing sen98 obs
vte_data[mc %like% "GOLDWATER" & congress == 98, votes]
vte_data[mc %like% "METZENBAUM" & congress == 98, votes]
vte_data[mc %like% "GORTON" & congress == 98, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 98, votes]


# search for who is dropped in 99
syd99_1 <- senator_year_data[congress == 99, ]
syd99_2 <- senator_year_data2[congress == 99, ]
syd99_2[, not_dropped := 1 * (icpsrLegis %in% syd99_1$icpsrLegis), ]
syd99_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen99 obs elected
sen_lep[thomas_name %like% "GOLDWATER" & congress == 99, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 99, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 99, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 99, votepct]

# how many votes by missing sen99 obs
vte_data[mc %like% "GOLDWATER" & congress == 99, votes]
vte_data[mc %like% "METZENBAUM" & congress == 99, votes]
vte_data[mc %like% "GORTON" & congress == 99, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 99, votes]


# search for who is dropped in 100
syd100_1 <- senator_year_data[congress == 100, ]
syd100_2 <- senator_year_data2[congress == 100, ]
syd100_2[, not_dropped := 1 * (icpsrLegis %in% syd100_1$icpsrLegis), ]
syd100_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen100 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 100, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 100, votepct]
sen_lep[thomas_name %like% "ZORINSKY" & congress == 100, votepct] # NA
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 100, votepct]

# how many votes by missing sen100 obs
vte_data[mc %like% "CONRAD" & congress == 100, votes]
vte_data[mc %like% "METZENBAUM" & congress == 100, votes]
vte_data[mc %like% "ZORINSKY" & congress == 100, votes] # 28
vte_data[mc %like% "LAUTENBERG" & congress == 100, votes]


# search for who is dropped in 101
syd101_1 <- senator_year_data[congress == 101, ]
syd101_2 <- senator_year_data2[congress == 101, ]
syd101_2[, not_dropped := 1 * (icpsrLegis %in% syd101_1$icpsrLegis), ]
syd101_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen101 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 101, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 101, votepct]
sen_lep[thomas_name %like% "ZORINSKY" & congress == 101, votepct] # NA
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 101, votepct]
sen_lep[thomas_name %like% "JEFFORDS" & congress == 101, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 101, votepct]

# how many votes by missing sen101 obs
vte_data[mc %like% "CONRAD" & congress == 101, votes]
vte_data[mc %like% "METZENBAUM" & congress == 101, votes]
vte_data[mc %like% "ZORINSKY" & congress == 101, votes] # NA
vte_data[mc %like% "LAUTENBERG" & congress == 101, votes]
vte_data[mc %like% "JEFFORDS" & congress == 101, votes]
vte_data[mc %like% "GORTON" & congress == 101, votes]


# search for who is dropped in 102
syd102_1 <- senator_year_data[congress == 102, ]
syd102_2 <- senator_year_data2[congress == 102, ]
syd102_2[, not_dropped := 1 * (icpsrLegis %in% syd102_1$icpsrLegis), ]
syd102_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen102 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 102, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 102, votepct]
sen_lep[thomas_name %like% "COATS" & congress == 102, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 102, votepct]
sen_lep[thomas_name %like% "JEFFORDS" & congress == 102, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 102, votepct]

# how many votes by missing sen102 obs
vte_data[mc %like% "CONRAD" & congress == 102, votes]
vte_data[mc %like% "METZENBAUM" & congress == 102, votes]
vte_data[mc %like% "COATS" & congress == 102, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 102, votes]
vte_data[mc %like% "JEFFORDS" & congress == 102, votes]
vte_data[mc %like% "GORTON" & congress == 102, votes]


# search for who is dropped in 103
syd103_1 <- senator_year_data[congress == 103, ]
syd103_2 <- senator_year_data2[congress == 103, ]
syd103_2[, not_dropped := 1 * (icpsrLegis %in% syd103_1$icpsrLegis), ]
syd103_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen103 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 103, votepct]
sen_lep[thomas_name %like% "METZENBAUM" & congress == 103, votepct]
sen_lep[thomas_name %like% "COATS" & congress == 103, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 103, votepct]
sen_lep[thomas_name %like% "JEFFORDS" & congress == 103, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 103, votepct]
sen_lep[thomas_name %like% "INHOFE" & congress == 103, votepct] # NA

# how many votes by missing sen103 obs
vte_data[mc %like% "CONRAD" & congress == 103, votes]
vte_data[mc %like% "METZENBAUM" & congress == 103, votes]
vte_data[mc %like% "COATS" & congress == 103, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 103, votes]
vte_data[mc %like% "JEFFORDS" & congress == 103, votes]
vte_data[mc %like% "GORTON" & congress == 103, votes]
vte_data[mc %like% "INHOFE" & congress == 103, votes] # 2


# search for who is dropped in 104
syd104_1 <- senator_year_data[congress == 104, ]
syd104_2 <- senator_year_data2[congress == 104, ]
syd104_2[, not_dropped := 1 * (icpsrLegis %in% syd104_1$icpsrLegis), ]
syd104_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen104 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 104, votepct]
sen_lep[thomas_name %like% "CAMPBELL" & congress == 104, votepct] # TWO OBS
sen_lep[thomas_name %like% "COATS" & congress == 104, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 104, votepct]
sen_lep[thomas_name %like% "JEFFORDS" & congress == 104, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 104, votepct]
sen_lep[thomas_name %like% "SHELBY" & congress == 104, votepct]

# how many votes by missing sen104 obs
vte_data[mc %like% "CONRAD" & congress == 104, votes]
vte_data[mc %like% "CAMPBELL" & congress == 104, votes] # TWO OBS
vte_data[mc %like% "COATS" & congress == 104, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 104, votes]
vte_data[mc %like% "JEFFORDS" & congress == 104, votes]
vte_data[mc %like% "GORTON" & congress == 104, votes]
vte_data[mc %like% "SHELBY" & congress == 104, votes]


# search for who is dropped in 105
syd105_1 <- senator_year_data[congress == 105, ]
syd105_2 <- senator_year_data2[congress == 105, ]
syd105_2[, not_dropped := 1 * (icpsrLegis %in% syd105_1$icpsrLegis), ]
syd105_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen105 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 105, votepct]
sen_lep[thomas_name %like% "CAMPBELL" & congress == 105, votepct]
sen_lep[thomas_name %like% "COATS" & congress == 105, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 105, votepct]
sen_lep[thomas_name %like% "JEFFORDS" & congress == 105, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 105, votepct]
sen_lep[thomas_name %like% "SHELBY" & congress == 105, votepct]

# how many votes by missing sen105 obs
vte_data[mc %like% "CONRAD" & congress == 105, votes]
vte_data[mc %like% "CAMPBELL" & congress == 105, votes]
vte_data[mc %like% "COATS" & congress == 105, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 105, votes]
vte_data[mc %like% "JEFFORDS" & congress == 105, votes]
vte_data[mc %like% "GORTON" & congress == 105, votes]
vte_data[mc %like% "SHELBY" & congress == 105, votes]


# search for who is dropped in 106
syd106_1 <- senator_year_data[congress == 106, ]
syd106_2 <- senator_year_data2[congress == 106, ]
syd106_2[, not_dropped := 1 * (icpsrLegis %in% syd106_1$icpsrLegis), ]
syd106_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen106 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 106, votepct]
sen_lep[thomas_name %like% "CAMPBELL" & congress == 106, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 106, votepct]
sen_lep[thomas_name %like% "JEFFORDS" & congress == 106, votepct]
sen_lep[thomas_name %like% "GORTON" & congress == 106, votepct]
sen_lep[thomas_name %like% "SHELBY" & congress == 106, votepct]

# how many votes by missing sen106 obs
vte_data[mc %like% "CONRAD" & congress == 106, votes]
vte_data[mc %like% "CAMPBELL" & congress == 106, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 106, votes]
vte_data[mc %like% "JEFFORDS" & congress == 106, votes]
vte_data[mc %like% "GORTON" & congress == 106, votes]
vte_data[mc %like% "SHELBY" & congress == 106, votes]


# search for who is dropped in 107
syd107_1 <- senator_year_data[congress == 107, ]
syd107_2 <- senator_year_data2[congress == 107, ]
syd107_2[, not_dropped := 1 * (icpsrLegis %in% syd107_1$icpsrLegis), ]
syd107_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen107 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 107, votepct]
sen_lep[thomas_name %like% "CAMPBELL" & congress == 107, votepct]
sen_lep[thomas_name %like% "BARKLEY" & congress == 107, votepct] # NA
sen_lep[thomas_name %like% "JEFFORDS" & congress == 107, votepct] # TWO OBS
sen_lep[thomas_name %like% "SHELBY" & congress == 107, votepct]

# how many votes by missing sen107 obs
vte_data[mc %like% "CONRAD" & congress == 107, votes]
vte_data[mc %like% "CAMPBELL" & congress == 107, votes]
vte_data[mc %like% "BARKLEY" & congress == 107, votes] # NA
vte_data[mc %like% "JEFFORDS" & congress == 107, votes] # TWO OBS
vte_data[mc %like% "SHELBY" & congress == 107, votes]


# search for who is dropped in 108
syd108_1 <- senator_year_data[congress == 108, ]
syd108_2 <- senator_year_data2[congress == 108, ]
syd108_2[, not_dropped := 1 * (icpsrLegis %in% syd108_1$icpsrLegis), ]
syd108_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen108 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 108, votepct]
sen_lep[thomas_name %like% "CAMPBELL" & congress == 108, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 108, votepct]
sen_lep[thomas_name %like% "SHELBY" & congress == 108, votepct]

# how many votes by missing sen108 obs
vte_data[mc %like% "CONRAD" & congress == 108, votes]
vte_data[mc %like% "CAMPBELL" & congress == 108, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 108, votes]
vte_data[mc %like% "SHELBY" & congress == 108, votes]


# search for who is dropped in 109
syd109_1 <- senator_year_data[congress == 109, ]
syd109_2 <- senator_year_data2[congress == 109, ]
syd109_2[, not_dropped := 1 * (icpsrLegis %in% syd109_1$icpsrLegis), ]
syd109_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen109 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 109, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 109, votepct]
sen_lep[thomas_name %like% "SHELBY" & congress == 109, votepct]

# how many votes by missing sen109 obs
vte_data[mc %like% "CONRAD" & congress == 109, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 109, votes]
vte_data[mc %like% "SHELBY" & congress == 109, votes]


# search for who is dropped in 110
syd110_1 <- senator_year_data[congress == 110, ]
syd110_2 <- senator_year_data2[congress == 110, ]
syd110_2[, not_dropped := 1 * (icpsrLegis %in% syd110_1$icpsrLegis), ]
syd110_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen110 obs elected
sen_lep[thomas_name %like% "CONRAD" & congress == 110, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 110, votepct]
sen_lep[thomas_name %like% "SHELBY" & congress == 110, votepct]

# how many votes by missing sen110 obs
vte_data[mc %like% "CONRAD" & congress == 110, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 110, votes]
vte_data[mc %like% "SHELBY" & congress == 110, votes]


# search for who is dropped in 111
syd111_1 <- senator_year_data[congress == 111, ]
syd111_2 <- senator_year_data2[congress == 111, ]
syd111_2[, not_dropped := 1 * (icpsrLegis %in% syd111_1$icpsrLegis), ]
syd111_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen111 obs elected
sen_lep[thomas_name %like% "BIDEN" & congress == 111, votepct] # NA
sen_lep[thomas_name %like% "CLINTON" & congress == 111, votepct] # NA
sen_lep[thomas_name %like% "CONRAD" & congress == 111, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 111, votepct]
sen_lep[thomas_name %like% "SALAZAR" & congress == 111, votepct] # NA
sen_lep[thomas_name %like% "SHELBY" & congress == 111, votepct]
sen_lep[thomas_name %like% "SPECTER" & congress == 111, votepct] # TWO OBS

# how many votes by missing sen111 obs
vte_data[mc %like% "BIDEN" & congress == 111, votes] # 2
vte_data[mc %like% "CLINTON" & congress == 111, votes] # 5
vte_data[mc %like% "CONRAD" & congress == 111, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 111, votes]
vte_data[mc %like% "SALAZAR" & congress == 111, votes] # 5
vte_data[mc %like% "SHELBY" & congress == 111, votes]
vte_data[mc %like% "SPECTER" & congress == 111, votes] # TWO OBS


# search for who is dropped in 112
syd112_1 <- senator_year_data[congress == 112, ]
syd112_2 <- senator_year_data2[congress == 112, ]
syd112_2[, not_dropped := 1 * (icpsrLegis %in% syd112_1$icpsrLegis), ]
syd112_2[not_dropped == 0, icpsrLegis, mc]

# were missing sen112 obs elected
sen_lep[thomas_name %like% "COATS" & congress == 111, votepct] # NA
sen_lep[thomas_name %like% "CONRAD" & congress == 111, votepct]
sen_lep[thomas_name %like% "LAUTENBERG" & congress == 111, votepct]
sen_lep[thomas_name %like% "SCHATZ" & congress == 111, votepct] # NA
sen_lep[thomas_name %like% "SHELBY" & congress == 111, votepct]

# how many votes by missing sen112 obs
vte_data[mc %like% "COATS" & congress == 111, votes] # NA
vte_data[mc %like% "CONRAD" & congress == 111, votes]
vte_data[mc %like% "LAUTENBERG" & congress == 111, votes]
vte_data[mc %like% "SCHATZ" & congress == 111, votes] # NA
vte_data[mc %like% "SHELBY" & congress == 111, votes]
