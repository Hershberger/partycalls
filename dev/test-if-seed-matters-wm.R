library(partycalls)
library(data.table)
library(ggplot2)
options(stringsAsFactors = FALSE)
load("inst/extdata/houKHfiles001-111.rdata")

congress_number <- 109
set.seed(1975242355)
replication <- code_party_calls(
  get(paste0("h", sprintf("%03.f", congress_number))),
  pval_threshold = 0.01,
  count_min = 25,
  count_max = 200,
  match_count_min = 5,
  sim_annealing = TRUE,
  random_seed = TRUE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = TRUE,
  type = "brglm")
replicate_partycalls_1 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding_1 = replication$party_call_coding$coding,
  replicate_pvals_1 = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))
set.seed(554242791)
replication <- code_party_calls(
  get(paste0("h", sprintf("%03.f", congress_number))),
  pval_threshold = 0.01,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm")
replicate_partycalls_2 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding_2 = replication$party_call_coding$coding,
  replicate_pvals_2 = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))
replicate_compare_109 <- merge(replicate_partycalls_1, replicate_partycalls_2,
  by = c("voteno", "congress"))
replicate_compare_109[, mean(replicate_coding_1 == replicate_coding_2)]

congress_number <- 97
set.seed(750969248)
replication <- code_party_calls(
  get(paste0("h", sprintf("%03.f", congress_number))),
  pval_threshold = 0.01,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm")
replicate_partycalls_1 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding_1 = replication$party_call_coding$coding,
  replicate_pvals_1 = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))
set.seed(1502141248)
replication <- code_party_calls(
  get(paste0("h", sprintf("%03.f", congress_number))),
  pval_threshold = 0.01,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm")
replicate_partycalls_2 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding_2 = replication$party_call_coding$coding,
  replicate_pvals_2 = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))
replicate_compare_97 <- merge(replicate_partycalls_1, replicate_partycalls_2,
  by = c("voteno", "congress"))
replicate_compare_97[, mean(replicate_coding_1 == replicate_coding_2)]

congress_number <- 102
set.seed(2143124376)
replication <- code_party_calls(
  get(paste0("h", sprintf("%03.f", congress_number))),
  pval_threshold = 0.01,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm")
replicate_partycalls_1 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding_1 = replication$party_call_coding$coding,
  replicate_pvals_1 = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))
set.seed(1688841345)
replication <- code_party_calls(
  get(paste0("h", sprintf("%03.f", congress_number))),
  pval_threshold = 0.01,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm")
replicate_partycalls_2 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding_2 = replication$party_call_coding$coding,
  replicate_pvals_2 = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))
replicate_compare_102 <- merge(replicate_partycalls_1, replicate_partycalls_2,
  by = c("voteno", "congress"))
replicate_compare_102[, mean(replicate_coding_1 == replicate_coding_2)]
