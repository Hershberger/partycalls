library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-5.R")
load("inst/extdata/houKHfiles001-111.rdata")
# load("inst/extdata/votedata-partycalls.RData")
# setDT(votedata)
# old_partycalls <- votedata[congress < 110, .(congress, voteno, partycall)]
# old_partycalls[, old_coding := "gray"]
# old_partycalls[partycall == TRUE, old_coding := "party call"]
# old_partycalls[partycall == FALSE, old_coding := "noncall"]
# old_partycalls[, partycall := NULL]
# old_partycalls[, congress := as.character(congress)]
# old_partycalls[, voteno := as.character(voteno)]
# whoheeds13 <- readstata13::read.dta13(
#   "inst/extdata/who-heeds-replication-archive.dta")
# setDT(whoheeds13)
# old_ideal_points <- whoheeds13[,
#   .(congress, icpsr, dem, old_ideal_point = ideal_partyfree)]


set.seed(1975242355)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(
    rc,
    pval_threshold = 0.01,
    count_min = 15,
    count_max = 150,
    match_count_min = 5,
    sim_annealing = FALSE,
    random_seed = FALSE,
    lopside_thresh = 0.65,
    drop_very_lopsided_votes = TRUE,
    return_pvals = TRUE,
    n_iterations_for_coding = 5,
    use_new_match_check = TRUE,
    type = "brglm",
    temperature_method = "curve",
    heat_time = 20,
    cool_time = 40,
    curve_mean = 10,
    curve_sd = 15,
    remove_flip_flop_votes_from_noncalls = FALSE,
    randomly_reassign_flip_flop_votes_from_noncalls = TRUE)
}

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls,
  file = "test_data/house_party_calls_flipflop.RData")


