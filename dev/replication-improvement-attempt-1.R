# library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-1.R")
load("inst/extdata/houKHfiles001-111.rdata")

set.seed(1584882915)

replication <- code_party_calls(
  h109,
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
  temperature_function = function(counter, n_votes)
    floor(n_votes * .2 * max(0, 1 - (abs(counter - 10) / 50)) ^ 2),
  remove_flip_flop_votes_from_noncalls = FALSE,
  randomly_reassign_flip_flop_votes_from_noncalls = TRUE)

replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding = replication$party_call_coding$coding,
  replicate_pvals = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))

replicate_partycalls[, mean(replicate_coding == "gray")]

load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress < 110, .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]
merge(old_partycalls, replicate_partycalls, by =
    c("voteno", "congress"))[, table(old_coding, replicate_coding)]

