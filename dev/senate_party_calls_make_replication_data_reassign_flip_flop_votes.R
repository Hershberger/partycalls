library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
load("inst/extdata/senate93-112.RData")

# load party calls data
load("test_data/senate_party_calls_replication_randomly_reassign_flip_flop_votes.RData")

names(senate_party_calls) <- paste0("sen", 93:112)

# load legislative effectiveness data
les_data <- read.csv("inst/extdata/93_113_senate_variables.csv")
setDT(les_data)

# get ideal points
new_partycalls <- rbindlist(lapply(senate_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

# get responsiveness rates
new_responsiveness <- rbindlist(lapply(93:112, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, senate_party_calls, chamber = "senate")
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
  file = "data/senate_party_calls_replication_reassign_flip_flop_votes.RData")

