library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

set.seed(1584882915)

code_party_calls(
  h095,
  pval_threshold = 0.01,
  count_min = 5,
  count_max = 150,
  match_count_min = 5,
  sim_annealing = TRUE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm")

replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding = replication$party_call_coding$coding,
  replicate_pvals = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))

load("inst/extdata/houKHfiles001-111.rdata")
replicate_partycalls[, mean(replicate_coding == "gray")]
merge(old_partycalls, replicate_partycalls, by =
    c("voteno", "congress"))[, table(old_coding, replicate_coding)]

