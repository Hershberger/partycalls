library(data.table)
library(parallel)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-3.R")
load("inst/extdata/houKHfiles001-111.rdata")

# set.seed(1975242355)
# code_party_calls_by_congress_number <- function(congress_number)
# {
#   cat("**** working on house", congress_number, "\n")
#   rc <- get(paste0("h", sprintf("%03.f", congress_number)))
#   rc <- code_party_calls(
#     rc,
#     pval_threshold = 0.01, tval_threshold = 2.32, count_min = 15,
#     count_max = 150, match_count_min = 150, sim_annealing = FALSE,
#     random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
#     drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
#     n_iterations_for_coding = 5, use_new_match_check = TRUE,
#     type = c("brglm", "lm", "glm"), use_classification_distance = FALSE,
#     remove_flip_flop_votes_from_noncalls = FALSE,
#     randomly_reassign_flip_flop_votes_from_noncalls = TRUE)
# }
#
# house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
# names(house_party_calls) <- paste0("hou", 93:109)
# save(house_party_calls,
#   file = "test_data/house_party_calls_flipflop.RData")

h103_test3 <- code_party_calls(h103,
  pval_threshold = 0.01, tval_threshold = 2.32, count_min = 15,
  count_max = 150, match_count_min = 150, sim_annealing = TRUE,
  random_seed = TRUE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
  drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
  n_iterations_for_coding = 5, use_new_match_check = TRUE,
  type = c("brglm", "lm", "glm"), use_classification_distance = FALSE,
  remove_flip_flop_votes_from_noncalls = FALSE,
  randomly_reassign_flip_flop_votes_from_noncalls = TRUE)
save(h103_test3,
     file = "test_data/h103_test3.RData")

h103_partycalls3 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", h103_test3$source),
  voteno = h103_test3$party_call_coding$voteno,
  replicate_coding = h103_test3$party_call_coding$coding)

h103_partycalls3$gray <-0
h103_partycalls3$gray[h103_partycalls3$replicate_coding == "gray"] <- 1
mean(h103_partycalls3$gray)
