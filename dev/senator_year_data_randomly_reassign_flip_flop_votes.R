library(partycalls)
library(data.table)
library(yaml)
options(stringsAsFactors = FALSE)
# make sure this dataset exists before running script
load("data/senator_year_data_emIRT_only.RData")

# load party calls data
load("test_data/senate_party_calls_replication_randomly_reassign_flip_flop_votes.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

new_partycalls <- rbindlist(lapply(senate_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

new_responsiveness <- rbindlist(lapply(93:112, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, senate_party_calls, chamber = "senate")
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsrLegis = icpsrLegis, party_free_ideal_point = pf_ideal,
    responsiveness_party_calls = 100 * responsiveness_party_calls,
    responsiveness_noncalls = 100 * responsiveness_noncalls,
    distance_from_floor_median = dist_from_floor_median,
    ideological_extremism = ideological_extremism)]
}))

senator_year_data[, c("party_free_ideal_point", "responsiveness_party_calls",
  "responsiveness_noncalls", "distance_from_floor_median",
  "ideological_extremism") := NULL]

senator_year_data <- merge(senator_year_data, new_responsiveness,
  by = c("icpsrLegis", "congress"))

# standardize variables
senator_year_data$party_free_ideal_point <-
  senator_year_data$party_free_ideal_point -
  mean(senator_year_data$party_free_ideal_point)

senator_year_data$party_free_ideal_point <-
  senator_year_data$party_free_ideal_point /
  sd(senator_year_data$party_free_ideal_point)

senator_year_data[caucus == "Republican", ideological_extremism := party_free_ideal_point]
senator_year_data[caucus == "Democrat", ideological_extremism := -1 * party_free_ideal_point]

save(senator_year_data,
  file = "data/senator_year_data_randomly_reassign_flip_flop_votes.RData")
