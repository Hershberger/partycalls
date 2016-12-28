library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
load("inst/extdata/houKHfiles001-111.rdata")

# load party calls data
load("test_data/house_party_calls_replication_hybrid.RData")

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
  DATA[, .(congress, icpsr = icpsrLegis, party_free_ideal_point = pf_ideal,
    pirate100 = 100 * responsiveness_party_calls,
    pfrate100 = 100 * responsiveness_noncalls,
    distance_from_floor_median = dist_from_floor_median,
    ideological_extremism = ideological_extremism)]
}))

new_whoheeds13 <- merge(les_data, new_responsiveness,
  by = c("congress", "icpsr"), all = TRUE)

setDT(new_whoheeds13)

# standardize variables
new_whoheeds13$party_free_ideal_point <-
  new_whoheeds13$party_free_ideal_point -
  mean(new_whoheeds13$party_free_ideal_point)

new_whoheeds13$party_free_ideal_point <-
  new_whoheeds13$party_free_ideal_point /
  sd(new_whoheeds13$party_free_ideal_point)

new_whoheeds13[dem == 0, ideological_extremism := party_free_ideal_point]
new_whoheeds13[dem == 1, ideological_extremism := -1 * party_free_ideal_point]

save(new_whoheeds13,
  file = "data/house_party_calls_replication_hybrid.RData")

