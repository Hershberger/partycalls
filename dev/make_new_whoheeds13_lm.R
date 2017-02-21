library(partycalls)
set.seed(1634538933, kind = "L'Ecuyer")

# load party calls data
load("test_data/house_party_calls_lm.RData")
names(house_party_calls) <- paste0("hou", 93:112)

# use LEP data from website to build baseline dataset
lep_data_93_110 <-
  readstata13::read.dta13("inst/extdata/LEPData93to110Congresses.dta")
setDT(lep_data_93_110)
lep_data_111_112 <-
  readstata13::read.dta13("inst/extdata/LEPData111to113Congresses.dta")
setDT(lep_data_111_112)
# drop congress 113
lep_data_111_112 <- lep_data_111_112[congress <= 112, ]
# load aggregate legislative effectiveness data for some missing variables
lep_aggregate <- readstata13::read.dta13("inst/extdata/LEP93to113.dta")
# drop Tim Ryan's first entry (shorter of two)
lep_data_93_110 <- subset(lep_data_93_110, !(congress == 108 & icpsr == 20343 &
    thomas_num == 7031))
lep_aggregate <- subset(lep_aggregate, !(congress == 108 & icpsr == 20343 &
    thomas_num == 7031))
# prep data for merge
setDT(lep_aggregate)
lep_aggregate <- lep_aggregate[congress %in% c(111:112), ]

stabb_to_drop <- c("PR", "DC", "GU", "VI", "AS", "MP")
lep_data_93_110 <- subset(lep_data_93_110, !(st_name %in% stabb_to_drop))
lep_data_111_112 <- subset(lep_data_111_112, !(st_name %in% stabb_to_drop))
lep_aggregate <- subset(lep_aggregate, !(st_name %in% stabb_to_drop))
lep_aggregate <- lep_aggregate[, .(congress, icpsr, elected, afam, latino,
  freshman, sophomore, south, leader)]

lep_data_111_112 <- merge(lep_data_111_112, lep_aggregate,
  by = c("congress", "icpsr"))

# select variables for analysis
lep_data_93_110 <- lep_data_93_110[, .(thomas_name, icpsr, congress, st_name,
  cd, dem, majority, female, afam, latino, votepct, speaker, chair, subchr,
  power, seniority, maj_leader, min_leader, south, les)]

lep_data_111_112 <- lep_data_111_112[, .(thomas_name, icpsr, congress, st_name,
  cd, dem, majority, female, afam, latino, votepct, speaker, chair, subchr,
  power, seniority, maj_leader, min_leader, south, les)]

# merge data sets
lep_data <- rbind(lep_data_93_110, lep_data_111_112)

# clean data and add variables needed
lep_data[thomas_name == "Albert, Carl", icpsr := 62]
lep_data[thomas_name == "Lambert, Blanche", icpsr := 29305]
lep_data[thomas_name == "Sekula Gibbs, Shelley", icpsr := 20541]

lep_data[icpsr == 62, dem := 1]
lep_data[icpsr == 20301, latino := 0]
lep_data[icpsr == 20301, afam := 0]

# fix south variable
south_stabb <- c("OK", "AR", "NC", "TX", "FL", "TN", "AL", "GA", "LA", "MS",
  "KY", "VA", "SC")
lep_data[is.na(south) == TRUE, south := 0]
lep_data[st_name %in% south_stabb, south := 1]

# missing votepct means appointee
# mark these as drop
lep_data[, drop := 0]
lep_data[is.na(votepct), drop := 1]

lep_data[, freshman := 0]
lep_data[seniority == 1, freshman := 1]

lep_data[, leader := 0]
lep_data[maj_leader == 1, leader := 1]
lep_data[min_leader == 1, leader := 1]

# create state_cd
state_fips <- fread("inst/extdata/statefips.csv")
setnames(lep_data, "st_name", "stabb")
state_fips <- state_fips[order(statename), ]
state_fips <- state_fips[stabb != "DC", ]
state_fips[, state_alphabetical_order := c(1:50)]
lep_data <- merge(lep_data, state_fips, by = "stabb")
lep_data[, state_cd := as.numeric(paste0(state_alphabetical_order,
  sprintf("%02.f", cd)))]

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

# # find missing dpres values
# member_year_data[is.na(dpres) == TRUE, .(icpsr, thomas_name, congress, state_cd)]
# # replace these with previous values
# member_year_data[state_cd == 3212 & congress == 93, dpres]
# member_year_data[state_cd == 3213 & congress == 93, dpres]
# member_year_data[state_cd == 3214 & congress == 93, dpres]
# member_year_data[state_cd == 3215 & congress == 93, dpres]
member_year_data[state_cd == 3212 & congress == 94, dpres := 84.42]
member_year_data[state_cd == 3213 & congress == 94, dpres := 51.45]
member_year_data[state_cd == 3214 & congress == 94, dpres := 52.22]
member_year_data[state_cd == 3215 & congress == 94, dpres := 32.29]

# create presidential vote share for same party candidate
member_year_data[dem == 1, pres_votepct := dpres]
member_year_data[dem == 0, pres_votepct := 100 - dpres]

# load replication data for committee data
old_committee <- foreign::read.dta("inst/extdata/who-heeds-replication-archive.dta")
setDT(old_committee)
old_best_committee <- old_committee[, .(congress, icpsr, bestgrosswart)]
setnames(old_best_committee, "icpsr", "icpsrLegis")

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
new_committee <- new_committee[drop != 1, ]

# fix icpsrLegis numbers
lep_new_data <- lep_data[congress >= 110,]
new_committee[, in_lep_data := 1 * (icpsrLegis %in% lep_new_data$icpsr)]
new_committee[in_lep_data == 0, .(congress, icpsrLegis, Name, stabb, cd)]
new_committee[icpsrLegis == 21169, icpsrLegis := 20524] # mike fitzpatrick
new_committee[icpsrLegis == 21144, icpsrLegis := 20725] # tim walburg
new_committee[icpsrLegis == 90901, icpsrLegis := 20901] # parker griffith
new_committee[icpsrLegis == 29335, icpsrLegis := 20959] # theodore deutch
new_committee[icpsrLegis == 21161, icpsrLegis := 29550] # steve chabot
new_committee[icpsrLegis == 39310, icpsrLegis := 20917] # ahn cao
new_committee[icpsrLegis == 15006, icpsrLegis := 20758] # gus bilirakis
# dan miller was not in congress at this time

# correct NA values
# no committee takes lower rank than worst committee
new_committee[is.na(committee) == TRUE, rank := 22]
new_committee[is.na(rank) == TRUE, rank := 22]

# get best committee for mc
new_best_committee <- new_committee[,
  .(bestgrosswart = min(rank, na.rm = TRUE)), .(congress, icpsrLegis, Name)]
new_best_committee[, best_grosswart := 22 - bestgrosswart]

new_best_committee <- new_best_committee[, .(congress, icpsrLegis, bestgrosswart)]

# merge in bestgrosswart data
setnames(member_year_data, "icpsr", "icpsrLegis")
best_committee <- rbind(old_best_committee, new_best_committee)
member_year_data <- merge(member_year_data, best_committee,
  by = c("icpsrLegis", "congress"), all.x = TRUE)

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
  file = "test_data/new_whoheeds13_lm.RData")

