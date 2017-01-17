library(partycalls)
set.seed(1634538933, kind = "L'Ecuyer")

# load party calls data
load("test_data/house_party_calls_emIRT_only.RData")
names(house_party_calls) <- paste0("hou", 93:112)

# load legislative effectiveness data
les_data <- readstata13::read.dta13("inst/extdata/LEP93to113.dta")
# drop Tim Ryan's first entry (shorter of two)
les_data <- subset(les_data, !(congress == 108 & icpsr == 20343 &
    thomas_num == 7031))
# prep data for merge
les_data <- subset(les_data, congress <= 112)
setDT(les_data)
setnames(les_data, "st_name", "stabb")
setnames(les_data, "icpsr", "icpsrLegis")
stabb_to_drop <- c("PR", "DC", "GU", "VI", "AS", "MP")
les_data[, drop_stabb := 1 * (stabb %in% stabb_to_drop)]
les_data <- les_data[drop_stabb == 0, ]
les_data <- les_data[, .(congress, state_cd, icpsrLegis, dem, majority, elected,
  female,  afam, latino, freshman, sophomore, south, chair, power, subchr, leader,
  votepct, dpres_pct, les, deleg_size, afam_dem, south_dem, thomas_name, stabb)]

# load jacobson presidential vote data
jacobson_pres <- gdata::read.xls("inst/extdata/HR4614.xls")
setDT(jacobson_pres)
# prep data for merge
jacobson_pres[, congress := calc_congress(year) + 1]
setnames(jacobson_pres, "stcd", "state_cd")
jacobson_pres <- jacobson_pres[congress >= 93 & congress <= 112, ]
jacobson_pres <- jacobson_pres[, .(congress, state_cd, dpres)]

member_year_data <- merge(les_data, jacobson_pres, by = c("congress", "state_cd"),
  all.x = TRUE)

# clean data and add variables needed
member_year_data[thomas_name == "Albert, Carl", icpsrLegis := 62]
member_year_data[thomas_name == "Lambert, Blanche", icpsrLegis := 29305]
member_year_data[thomas_name == "Sekula Gibbs, Shelley", icpsrLegis := 20541]

member_year_data[icpsrLegis == 20301, latino := 0]
member_year_data[icpsrLegis == 20301, afam := 0]

member_year_data[icpsrLegis == 62, dem := 1]

member_year_data[is.na(state_cd) == TRUE, dpres := dpres_pct]

member_year_data[dem == 1, pres_votepct := dpres]
member_year_data[dem == 0, pres_votepct := 100 - dpres]


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

