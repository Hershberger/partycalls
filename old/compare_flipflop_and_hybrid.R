library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-3.R")
load("inst/extdata/houKHfiles001-111.rdata")

# first seed, hybrid then sim annealing
set.seed(1975242355)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(
    rc,
    pval_threshold = 0.01, tval_threshold = 2.32, count_min = 15,
    count_max = 150, match_count_min = 150, sim_annealing = TRUE,
    random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
    drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
    n_iterations_for_coding = 5, use_new_match_check = TRUE,
    type = c("brglm", "lm", "glm"), use_classification_distance = FALSE,
    remove_flip_flop_votes_from_noncalls = FALSE,
    randomly_reassign_flip_flop_votes_from_noncalls = TRUE)
}

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls,
     file = "test_data/house_party_calls_hybrid_seed1.RData")

rm(list=ls())

library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-3.R")
load("inst/extdata/houKHfiles001-111.rdata")

set.seed(1975242355)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(
    rc,
    pval_threshold = 0.01, tval_threshold = 2.32, count_min = 15,
    count_max = 150, match_count_min = 150, sim_annealing = TRUE,
    random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
    drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
    n_iterations_for_coding = 5, use_new_match_check = TRUE,
    type = c("brglm", "lm", "glm"), use_classification_distance = FALSE,
    remove_flip_flop_votes_from_noncalls = FALSE,
    randomly_reassign_flip_flop_votes_from_noncalls = FALSE)
}

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls,
     file = "test_data/house_party_calls_annealing_seed1.RData")

rm(list=ls())

# second seed, hybrid then sim annealing
set.seed(5532425791)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(
    rc,
    pval_threshold = 0.01, tval_threshold = 2.32, count_min = 15,
    count_max = 150, match_count_min = 150, sim_annealing = TRUE,
    random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
    drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
    n_iterations_for_coding = 5, use_new_match_check = TRUE,
    type = c("brglm", "lm", "glm"), use_classification_distance = FALSE,
    remove_flip_flop_votes_from_noncalls = FALSE,
    randomly_reassign_flip_flop_votes_from_noncalls = TRUE)
}

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls,
     file = "test_data/house_party_calls_hybrid_seed2.RData")

rm(list=ls())

library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-3.R")
load("inst/extdata/houKHfiles001-111.rdata")

set.seed(5532425791)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(
    rc,
    pval_threshold = 0.01, tval_threshold = 2.32, count_min = 15,
    count_max = 150, match_count_min = 150, sim_annealing = TRUE,
    random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
    drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
    n_iterations_for_coding = 5, use_new_match_check = TRUE,
    type = c("brglm", "lm", "glm"), use_classification_distance = FALSE,
    remove_flip_flop_votes_from_noncalls = FALSE,
    randomly_reassign_flip_flop_votes_from_noncalls = FALSE)
}

house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls,
     file = "test_data/house_party_calls_annealing_seed2.RData")
