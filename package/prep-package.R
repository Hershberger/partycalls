library(partycalls)

# Code party calls for the Senate
if (!file.exists("results/senate_party_calls_lm.RData")) {
  set.seed(846492672)
  senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
    chamber = "senate", pval_threshold = 0.05, type = "lm")
  names(senate_party_calls) <- paste0("sen", 93:112)
  save(senate_party_calls, file = "results/senate_party_calls_lm.RData",
    compress = "bzip2")
} else {
  load("results/senate_party_calls_lm.RData")
}

# Code party calls for the House
if (!file.exists("results/house_party_calls_lm.RData")) {
  set.seed(703851427)
  house_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
    chamber = "house", pval_threshold = 0.01, type = "lm")
  names(house_party_calls) <- paste0("hou", 93:112)
  save(house_party_calls, file = "results/house_party_calls_lm.RData",
    compress = "bzip2")
} else {
  load("results/house_party_calls_lm.RData")
}

# Code party calls for the Senate (bias-reduced method)
if (!file.exists("results/senate_party_calls_brglm.RData")) {
  set.seed(2081425373)
  senate_party_calls_brglm <- lapply(93:112,
    code_party_calls_by_congress_number,
    chamber = "senate", pval_threshold = 0.05, type = "brglm")
  names(senate_party_calls_brglm) <- paste0("sen", 93:112)
  save(senate_party_calls_brglm,
    file = "results/senate_party_calls_brglm.RData",
    compress = "bzip2")
} else {
  load("results/senate_party_calls_brglm.RData")
}

# Code party calls for the House (bias-reduced method)
if (!file.exists("results/house_party_calls_lm.RData")) {
  set.seed(1189028224)
  house_party_calls_brglm <- lapply(93:112, code_party_calls_by_congress_number,
    chamber = "house", pval_threshold = 0.01, type = "brglm")
  names(house_party_calls_brglm) <- paste0("hou", 93:112)
  save(house_party_calls_brglm, file = "results/house_party_calls_brglm.RData",
    compress = "bzip2")
} else {
  load("results/house_party_calls_brglm.RData")
}

# Build Senate data
if (!file.exists("results/senator_year_data.RData")) {
  source("package/prep-senate-data.R")
} else {
  load("results/senator_year_data.RData")
}

if (!file.exists("results/senate_data.RData")) {
  responsiveness_data <- rbindlist(lapply(93:112, function(congress) {
    cat("\r", congress)
    rc <- make_member_year_data(congress, senate_party_calls, chamber = "senate")
    DATA <- rc$member_year_data
    DATA[, .(congress,
      icpsrLegis = icpsrLegis,
      party_free_ideal_point = pf_ideal,
      pirate100 = 100 * responsiveness_party_calls,
      pfrate100 = 100 * responsiveness_noncalls,
      ideological_extremism)]
  }))

  setnames(senator_year_data, "caucus_majority", "maj")

  senate_data <- merge(
    senator_year_data,
    responsiveness_data,
    by = c("congress", "icpsrLegis"), all = TRUE)
  setDT(senate_data)

  # correct buckley extremism
  senate_data[icpsrLegis == 13100, ideological_extremism := abs(party_free_ideal_point)]

  # drop those without pirate100 values
  senate_data[is.na(pirate100) == TRUE, drop := 1]
  senate_data[is.na(pfrate100) == TRUE, drop := 1]

  senate_data <- senate_data[drop == 0, ]
  senate_data[, dem := NA_real_]
  senate_data[caucus == "Democrat", dem := 1]
  senate_data[caucus == "Republican", dem := 0]
  senate_data[, vote_share := vote_share * 100]
  senate_data[, pres_vote_share := pres_vote_share * 100]
  setnames(senate_data,
    c("maj"),
    c("majority"))

  old_names <- c("pirate100", "pfrate100", "afam")
  new_names <- c("responsiveness_to_party_calls", "baseline_rate",
    "african_american")
  setnames(senate_data, old_names, new_names)
  senate_data[congress == 107, majority := dem]

  # drop Jeffords from 107
  senate_data <- senate_data[!(congress == 107 & icpsrLegis %in% c(14240, 94240))]

  save(senate_data, file = "results/senate_data.RData")
} else {
  load("results/senate_data.RData")
}

# Build House data

if (!file.exists("results/house_data.RData")) {

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
  lep_aggregate <- lep_aggregate[, .(congress, icpsr, afam, latino,
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
  jacobson <- gdata::read.xls("inst/extdata/HR4614.xls")
  setDT(jacobson)
  # prep data for merge
  jacobson[, congress := calc_congress(year) + 1]
  setnames(jacobson, "stcd", "state_cd")
  jacobson1 <- jacobson[congress >= 93 & congress <= 112, ]
  jacobson1 <- jacobson[, .(congress, state_cd, dv, dpres, po1, po2.)]
  jacobson2 <- jacobson[congress >= 94 & congress <= 113, ]
  jacobson2[, congress := congress - 1]
  jacobson2 <- jacobson[, .(congress, state_cd, dvp)]


  member_year_data <- merge(lep_data, jacobson1,
    by = c("congress", "state_cd"),
    all.x = TRUE)
  member_year_data <- merge(member_year_data, jacobson2,
    by = c("congress", "state_cd"),
    all.x = TRUE)
  member_year_data[is.na(dv) == TRUE, dv := dvp]

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

  # fix dem and majority variables for analysis

  setnames(member_year_data, "icpsr", "icpsrLegis")

  # Eugene Atkinson, party changer
  member_year_data[icpsrLegis == 94602 & congress == 97, dem := 0]
  member_year_data[icpsrLegis == 94602 & congress == 97, majority := 0]
  # Phil Gramm, party changer
  member_year_data[icpsrLegis == 14628 & congress == 98, dem := 0]
  member_year_data[icpsrLegis == 14628 & congress == 98, majority := 0]
  # Bill Grant, party changer
  member_year_data[icpsrLegis == 15415 & congress == 101, dem := 0]
  member_year_data[icpsrLegis == 15415 & congress == 101, majority := 0]
  # Bill Redmond, miscoded
  member_year_data[icpsrLegis == 29772 & congress == 105, dem := 0]
  member_year_data[icpsrLegis == 29772 & congress == 105, majority := 1]
  # J. Randy Forbes, miscoded
  member_year_data[icpsrLegis == 20143 & congress == 107, dem := 0]
  member_year_data[icpsrLegis == 20143 & congress == 107, majority := 1]
  # John Moakley, miscoded
  member_year_data[icpsrLegis == 14039 & congress == 93, dem := 1]
  member_year_data[icpsrLegis == 14039 & congress == 93, majority := 1]
  # Joseph Smith, miscoded
  member_year_data[icpsrLegis == 14876 & congress == 97, dem := 1]
  member_year_data[icpsrLegis == 14876 & congress == 97, majority := 1]
  # Jill Long, miscoded
  member_year_data[icpsrLegis == 15631 & congress == 101, dem := 1]
  member_year_data[icpsrLegis == 15631 & congress == 101, majority := 1]
  # John Oliver, miscoded
  member_year_data[icpsrLegis == 29123 & congress == 102, dem := 1]
  member_year_data[icpsrLegis == 29123 & congress == 102, dem := 1]
  # Bernie Sanders, independent who we don't want to count as Republican
  member_year_data[icpsrLegis == 29147 & congress >= 102, dem := 1]
  member_year_data[icpsrLegis == 29147 & congress >= 102,
    majority := abs(majority - 1)]

  # there are minority party members listed as chairs, fix this
  member_year_data[icpsrLegis == 11036 & congress == 100, chair := 0]
  member_year_data[icpsrLegis == 14829 & congress == 102, chair := 0]
  member_year_data[icpsrLegis == 14248 & congress == 107, chair := 0]


  # create presidential vote share for same party candidate
  member_year_data[dem == 1, pres_vote_share := dpres]
  member_year_data[dem == 0, pres_vote_share := 100 - dpres]
  member_year_data[dem == 1, vote_share := dv]
  member_year_data[dem == 0, vote_share := 100 - dv]


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
  best_committee <- rbind(old_best_committee, new_best_committee)
  member_year_data <- merge(member_year_data, best_committee,
    by = c("icpsrLegis", "congress"), all.x = TRUE)
  member_year_data[is.na(bestgrosswart) == TRUE, bestgrosswart := 0]

  # get responsiveness rates
  new_responsiveness <- rbindlist(lapply(93:112, function(congress) {
    cat("\r", congress)
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

  # # correct some values
  # check_dem <- new_whoheeds13[dem == 1 &
  #   ideological_extremism != -party_free_ideal_point, ]
  # check_rep <- new_whoheeds13[dem == 0 &
  #   ideological_extremism != party_free_ideal_point, ]

  new_whoheeds13[dem == 1 & ideological_extremism != -party_free_ideal_point,
    ideological_extremism := -party_free_ideal_point]
  new_whoheeds13[dem == 0 & ideological_extremism != party_free_ideal_point,
    ideological_extremism := party_free_ideal_point]

  # drop members with missing values in variables used for analysis
  new_whoheeds13[, drop := 0]
  new_whoheeds13[is.na(majority) == TRUE, drop := 1]
  new_whoheeds13[is.na(pirate100) == TRUE, drop := 1]
  new_whoheeds13[is.na(pfrate100) == TRUE, drop := 1]
  new_whoheeds13[is.na(ideological_extremism) == TRUE, drop := 1]
  new_whoheeds13[is.na(party_free_ideal_point) == TRUE, drop := 1]

  # drop uneeded variables
  new_whoheeds13[, `:=`(c("fips", "statename", "dvp", "po1", "po2.",
    "state_alphabetical_order"), NULL)]

  # drop appointees
  new_whoheeds13[is.na(votepct) == TRUE, drop := 1]

  # party changers and special elections miscoded; correct them
  new_whoheeds13[vote_share < 50 & drop == 0, vote_share := 100 - vote_share]

  house_data <- new_whoheeds13[drop == 0, ]

  house_data[is.na(vote_share), vote_share := 100]
  setnames(house_data,
    c("bestgrosswart", "power"),
    c("best_committee", "power_committee"))
  old_names <- c("pirate100", "pfrate100", "afam")
  new_names <- c("responsiveness_to_party_calls", "baseline_rate",
    "african_american")
  setnames(house_data, old_names, new_names)

  # drop duplicates cause by committee merge
  house_data <- house_data[!(congress == 111 & icpsrLegis == 20535 & best_committee == 7)]
  house_data <- house_data[!(congress == 111 & icpsrLegis == 20932 & best_committee == 9)]
  house_data <- house_data[!(congress == 112 & icpsrLegis == 20932 & best_committee == 13)]
  house_data <- house_data[!(congress == 112 & icpsrLegis == 20958 & best_committee == 7)]

  save(house_data, file = "results/house_data.RData")
} else {
  load("results/house_data.RData")
}

if (!file.exists("results/coding_record.RData")) {
  house_coding_record <- data.table(congress = 93:112)
  house_coding_record[, party_calls := sapply(93:112, function(x)
    length(get_party_calls(house_party_calls[[paste0("hou", x)]])))]
  house_coding_record[, noncalls := sapply(93:112, function(x)
    length(get_noncalls(house_party_calls[[paste0("hou", x)]])))]
  house_coding_record[, gray_vote_count := sapply(93:112, function(x)
    length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))]
  house_coding_record[, percent_party_calls :=
      100 * party_calls / (party_calls + noncalls)]
  house_coding_record[, majority := "Republican"]
  house_coding_record[congress %in% c(93:103, 110:111), majority := "Democrat"]
  house_coding_record[, chamber := "House"]

  senate_coding_record <- data.table(congress = 93:112)
  senate_coding_record[, party_calls := sapply(93:112, function(x)
    length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))]
  senate_coding_record[, noncalls := sapply(93:112, function(x)
    length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))]
  senate_coding_record[, gray_vote_count := sapply(93:112, function(x)
    length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))]
  senate_coding_record[, percent_party_calls :=
      100 * party_calls / (party_calls + noncalls)]
  senate_coding_record[, majority := "Republican"]
  senate_coding_record[congress %in% c(93:96, 100:103, 107, 110:112),
    majority := "Democrat"]
  senate_coding_record[, chamber := "Senate"]

  coding_record <- rbind(house_coding_record, senate_coding_record)
  save(house_coding_record, senate_coding_record, coding_record,
    file = "results/coding_record.RData")
} else {
  load("results/coding_record.RData")
}



#----#
#
#Add details

details <- read.csv("inst/extdata/HSall_rollcalls.csv")
setDT(details)
setorder(details, chamber, congress, rollnumber)
for (cong in 93:112) {
  ok <- as.numeric(substr(colnames(
    house_party_calls[[cong - 92]]$votes), 6, 100))
  house_party_calls[[cong - 92]]$vote.data <-
    details[chamber == "House" & congress == cong &
        rollnumber %in% ok]

  ok <- as.numeric(substr(colnames(
    senate_party_calls[[cong - 92]]$votes), 6, 100))
  senate_party_calls[[cong - 92]]$vote.data <-
    details[chamber == "Senate" & congress == cong &
        rollnumber %in% ok]
}

devtools::use_data(
  coding_record, house_data, senate_data,
  house_party_calls, senate_party_calls,
  overwrite = TRUE)
devtools::build()
devtools::install()
