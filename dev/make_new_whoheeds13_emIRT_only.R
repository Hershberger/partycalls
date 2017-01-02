library(partycalls)
set.seed(1634538933, kind = "L'Ecuyer")

# load party calls data
load("test_data/house_party_calls_emIRT_only.RData")
names(house_party_calls) <- paste0("hou", 93:109)

# load legislative effectiveness data
les_data <- readstata13::read.dta13("inst/extdata/LEP93to113.dta")
# drop Tim Ryan's first entry (shorter of two)
les_data <- subset(les_data, !(congress == 108 & icpsr == 20343 &
    thomas_num == 7031))
les_data <- subset(les_data, congress <= 109)

# get responsiveness rates
new_responsiveness <- rbindlist(lapply(93:109, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, house_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress,
    icpsr = icpsrLegis,
    party_free_ideal_point = pf_ideal,
    pirate100 = 100 * responsiveness_party_calls,
    pfrate100 = 100 * responsiveness_noncalls,
    ideological_extremism)]
}))

new_whoheeds13 <- merge(les_data, new_responsiveness,
  by = c("congress", "icpsr"), all = TRUE)
setDT(new_whoheeds13)

save(new_whoheeds13,
  file = "test_data/new_whoheeds13_emIRT_only.RData")

