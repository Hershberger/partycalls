library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
load("inst/extdata/houKHfiles001-111.rdata")

# load party calls data
load("test_data/house_party_calls_replication_randomly_reassign_flip_flop_votes.RData")

names(house_party_calls) <- paste0("hou", 93:109)

# load legislative effectiveness data
les_data <- readstata13::read.dta13("inst/extdata/LEP93to113.dta")

# get ideal points
new_partycalls <- rbindlist(lapply(house_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

# get responsiveness rates
new_responsiveness <- rbindlist(lapply(93:109, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, house_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsr = icpsrLegis, new_ideal_point = pf_ideal,
    new_pirate100 = 100 * responsiveness_party_calls,
    new_pfrate100 = 100 * responsiveness_noncalls,
    new_distance_from_floor_median = dist_from_floor_median,
    new_ideological_extremism = ideological_extremism)]
}))

new_whoheeds13 <- merge(les_data, new_responsiveness,
  by = c("congress", "icpsr"), all = TRUE)

save(new_whoheeds13,
  file = "data/house_party_calls_replication_reassign_flip_flop_votes.RData")

