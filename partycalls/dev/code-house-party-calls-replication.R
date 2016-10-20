library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

set.seed(1584882915)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(rc, tval_threshold = 2.32,
    count_min = 10,
    count_max = 50, match_count_min = 6, sim_annealing = FALSE,
    random_seed = FALSE, lopside_thresh = 0.65,
    drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
    n_iterations_for_coding = 5, use_new_match_check = FALSE)
}
house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls, file = "inst/extdata/house_party_calls.RData")
