# set options and load necessary packages
options(stringsAsFactors = FALSE)
library(data.table)
library(pscl)
library(emIRT)
library(partycalls)


load("test_data/house_party_calls_flipflop.RData")
house_party_calls_ff <- house_party_calls
hyd_list <- lapply(93:109, make_member_year_data, house_party_calls_ff)
house_year_data_ff <- rbindlist(lapply(hyd_list, function(x) x$member_year_data))
setnames(house_year_data, "state", "stabb")

house_year_data_ff$response_check <- 0
house_year_data_ff$response_check[house_year_data_ff$responsiveness_party_calls
  < house_year_data_ff$responsiveness_noncalls] <- 1
mean(house_year_data_ff$response_check)

load("test_data/house_party_calls_hybrid_seed1.RData")
house_party_calls_hy <- house_party_calls
hyd_list <- lapply(93:109, make_member_year_data, house_party_calls_hy)
house_year_data_hybrid <- rbindlist(lapply(hyd_list, function(x) x$member_year_data))

house_year_data_hybrid$response_check <- 0
house_year_data_hybrid$response_check[house_year_data_hybrid$responsiveness_party_calls
  < house_year_data_hybrid$responsiveness_noncalls] <- 1
mean(house_year_data_hybrid$response_check)


load("inst/extdata/house_party_calls_replication.RData")
house_party_calls <- house_party_calls
hyd_list <- lapply(93:109, make_member_year_data, house_party_calls)
house_year_data <- rbindlist(lapply(hyd_list, function(x) x$member_year_data))
setnames(house_year_data, "state", "stabb")

house_year_data$response_check <- 0
house_year_data$response_check[house_year_data$responsiveness_party_calls
  < house_year_data$responsiveness_noncalls] <- 1
mean(house_year_data$response_check)

