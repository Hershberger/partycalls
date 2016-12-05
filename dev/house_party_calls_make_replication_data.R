library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

# load party calls data
load("inst/extdata/")
# load legislative effectiveness data
les_data <- readstata13::read.dta13("inst/extdata/LEP93to113.dta")

# get ideal points
new_ideal_points <- rbindlist(lapply(c(93:103, 104:109), function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, house_party_calls)
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsr = icpsrLegis, new_ideal_point = pf_ideal)]
}))

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

