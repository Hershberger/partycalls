library(partycalls)
set.seed(1634538933, kind = "L'Ecuyer")

# load party calls data
load("test_data/house_party_calls_emIRT_only.RData")
names(house_party_calls) <- paste0("hou", 93:112)

# load legislative effectiveness data
lep_data <- readstata13::read.dta13("inst/extdata/LEP93to113.dta")
# drop Tim Ryan's first entry (shorter of two)
lep_data <- subset(lep_data, !(congress == 108 & icpsr == 20343 &
    thomas_num == 7031))
# prep data for merge
lep_data <- subset(lep_data, congress <= 112)
setDT(lep_data)
setnames(lep_data, "st_name", "stabb")
setnames(lep_data, "icpsr", "icpsrLegis")
stabb_to_drop <- c("PR", "DC", "GU", "VI", "AS", "MP")
lep_data[, drop_stabb := 1 * (stabb %in% stabb_to_drop)]
lep_data <- lep_data[drop_stabb == 0, ]
lep_data <- lep_data[, .(congress, state_cd, stabb, cd, thomas_name, icpsrLegis, dem, majority, elected,
  female, afam, latino, freshman, sophomore, seniority, south, chair, power,
  subchr, leader, votepct, dpres_pct, les, deleg_size, afam_dem, south_dem,
  thomas_name)]

# might need to use LEP data from website
lep_data_93_110 <- readstata13::read.dta13("inst/extdata/LEPData93to110Congresses.dta")
setDT(lep_data_93_110)
lep_data_111_112 <- readstata13::read.dta13("inst/extdata/LEPData111to113Congresses.dta")

# figure out who is missing state_cd and repair it
state_fips <- fread("inst/extdata/statefips.csv")
lep_data <- merge(lep_data, state_fips, by = "stabb")
lep_data[is.na(state_cd) == TRUE,
  state_cd := as.numeric(paste0(fips, sprintf("%02.f", cd)))]

# load jacobson presidential vote data
jacobson_pres <- gdata::read.xls("inst/extdata/HR4614.xls")
setDT(jacobson_pres)
# prep data for merge
jacobson_pres[, congress := calc_congress(year) + 1]
setnames(jacobson_pres, "stcd", "state_cd")
jacobson_pres <- jacobson_pres[congress >= 93 & congress <= 112, ]
jacobson_pres <- jacobson_pres[, .(congress, state_cd, dpres)]

member_year_data <- merge(lep_data, jacobson_pres,
  by = c("congress", "state_cd"),
  all.x = TRUE)


# load replication data
old_whoheeds13 <- foreign::read.dta("inst/extdata/who-heeds-replication-archive.dta")
setDT(old_committee)

# clean data and add variables needed
afam_check <- lep_data[is.na(afam) == TRUE, ]
latino_check <- lep_data[is.na(afam) == TRUE, ]
maj_check <- lep_data[is.na(majority) == TRUE, ]
dem_check <- lep_data[is.na(dem) == TRUE, ]
leader_check <- lep_data[is.na(leader) == TRUE, ]
freshman_check <- lep_data[is.na(freshman) == TRUE, ]
seniority_check <- lep_data[is.na(seniority) == TRUE, ]
south_check <- lep_data[is.na(south) == TRUE, ]

member_year_data[thomas_name == "Albert, Carl", icpsrLegis := 62]
member_year_data[thomas_name == "Lambert, Blanche", icpsrLegis := 29305]
member_year_data[thomas_name == "Sekula Gibbs, Shelley", icpsrLegis := 20541]

member_year_data[icpsrLegis == 20301, latino := 0]
member_year_data[icpsrLegis == 20301, afam := 0]

member_year_data[icpsrLegis == 62, dem := 1]

member_year_data[is.na(state_cd) == TRUE, dpres := dpres_pct]

member_year_data[dem == 1, pres_votepct := dpres]
member_year_data[dem == 0, pres_votepct := 100 - dpres]




# get old committee data
old_best_committee <- old_whoheeds13[, .(congress, icpsr, bestgrosswart)]

# get stewart committee data for congress 110-112
new_committee <- fread("inst/extdata/house_assignments_103-114-1.csv")
setnames(new_committee, "Congress", "congress")
setnames(new_committee, "Committee code", "code")
setnames(new_committee, "ID #", "icpsrLegis")
setnames(new_committee, "Committee Name", "committee_name")
setnames(new_committee, "State Name", "stabb")
setnames(new_committee, "CD", "cd")
setnames(new_committee, "Maj/Min", "maj")
new_committee_value <- fread("inst/extdata/committee_values_house_110-112.csv")
new_committee <- merge(new_committee, new_committee_value, by = "code",
  all.x = TRUE)
new_committee <- new_committee[is.na(congress) == FALSE,]
new_committee <- new_committee[congress >= 110, ]
new_committee <- new_committee[congress <= 112, ]
new_committee[, drop := 1 * (stabb %in% stabb_to_drop)]
new_committee <- new_committee[drop != 1]

# fix icpsrLegis numbers
lep_new_data <- lep_data[congress >= 110,]
new_committee[, in_lep_data := 1 * (icpsrLegis %in% lep_new_data$icpsr)]
new_committee[in_lep_data == 0, .(congress, icpsrLegis, Name)]
new_committee[icpsrLegis == 21169, icpsrLegis := 20524] # mike fitzpatrick
new_committee[icpsrLegis == 21144, icpsrLegis := 20725] # tim walburg
new_committee[icpsrLegis == 90901, icpsrLegis := 20901] # parker griffith
new_committee[icpsrLegis == 29335, icpsrLegis := 20959] # theodore deutch
new_committee[icpsrLegis == 21161, icpsrLegis := 29550] # steve chabot
new_committee[icpsrLegis == 39310, icpsrLegis := 20917] # ahn cao
new_committee[icpsrLegis == 15006, icpsrLegis := 20758] # gus bilirakis

# correct NA values
new_committee[is.na(committee) == TRUE, rank := 21]
new_committee[is.na(rank) == TRUE, rank := 21]
leader_no_committee <- c("SPEAKER", "MAJORITY LEADER", "MINORITY LEADER",
  "MAJORITY WHIP", "MINORITY WHIP", "Speaker", "Majority Leader",
  "Minority Leader", "Majority Whip", "Minority Whip")
new_committee_leader_check <- new_committee[rank == 21, ]
new_committee_leader_check[, leader := 0]
new_committee_leader_check[, unique(committee_name)]
new_committee_leader_check[, rank_2 := 21]
new_committee_leader_check[committee_name %in% leader_no_committee,
  rank_2 := 0]
leader_committee <-new_committee_leader_check[, .(leader_check = max(rank_2)),
  .(congress, icpsrLegis, Name)]
leader_committee <- leader_committee[leader_check == 0, ]

# get best committee for mc
new_best_committee <- new_committee[,
  .(bestgrosswart = min(rank, na.rm = TRUE)), .(congress, icpsrLegis, Name)]
new_best_committee <- merge(new_best_committee, leader_committee,
  by = c("congress", "icpsrLegis", "Name"), all.x = TRUE)
new_best_committee[leader_check == 0, bestgrosswart := 0]
new_best_committee <- new_best_committee[, .(congress, icpsr, bestgrosswart)]

# merge in bestgrosswart data
best_committee <- rbind(old_committee, new_best_committee)
member_year_data <- merge(member_year_data, best_committee,
  by = c("icpsrLegis", congress), all.x = TRUE)

# get responsiveness rates
new_responsiveness <- rbindlist(lapply(93:112, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, house_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress,
    icpsrLegis,
    party_free_ideal_point = pf_ideal,
    pirate100 = 100 * responsiveness_party_calls,
    pfrate100 = 100 * responsiveness_noncalls,
    ideological_extremism)]
}))

new_whoheeds13 <- merge(member_year_data, new_responsiveness,
  by = c("congress", "icpsrLegis"), all = TRUE)
setDT(new_whoheeds13)

save(new_whoheeds13,
  file = "test_data/new_whoheeds13_emIRT_only.RData")

