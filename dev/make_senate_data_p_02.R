library(partycalls)
set.seed(181925809, kind = "L'Ecuyer")

# load party calls data
load("test_data/senate_party_calls_p_02.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

# load senator_year_data here
load("test_data/senator_year_data.RData")

# get responsiveness rates
responsiveness_data <- rbindlist(lapply(93:112, function(congress) {
  cat(congress, " ")
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

save(senate_data,
  file = "test_data/senate_data_p_02.RData")

