library(partycalls)
set.seed(1634538933, kind = "L'Ecuyer")

# load party calls data
load("test_data/senate_party_calls_emIRT_only.RData")
names(senate_party_calls_replication_emIRT_only) <- paste0("sen", 93:109)

# load senator_year_data here
load("test_data/senator_year_data.RData")

# get responsiveness rates
responsiveness_data <- rbindlist(lapply(93:109, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, senate_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress,
    icpsr = icpsrLegis,
    party_free_ideal_point = pf_ideal,
    pirate100 = 100 * responsiveness_party_calls,
    pfrate100 = 100 * responsiveness_noncalls,
    ideological_extremism)]
}))

senate_data <- merge(
  senator_year_data,
  responsiveness_data,
  by = c("congress", "icpsr"), all = TRUE)
setDT(senate_data)

save(senate_data,
  file = "test_data/senate_data_emIRT_only.RData")

