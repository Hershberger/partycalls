library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
load("inst/extdata/senate93-112.RData")
set.seed(1584882915)
code_party_calls(sen93, pval_threshold = 0.01017044, count_min = 3,
  count_max = 3, match_count_min = 3, sim_annealing = FALSE,
  random_seed = FALSE, lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE)
code_party_calls(sen93, pval_threshold = 0.01017044, count_min = 3,
  count_max = 3, match_count_min = 3, sim_annealing = TRUE,
  random_seed = FALSE, lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE)
code_party_calls(sen93, pval_threshold = 0.01017044, count_min = 3,
  count_max = 3, match_count_min = 3, sim_annealing = FALSE,
  random_seed = TRUE, lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE)
code_party_calls(sen93, pval_threshold = 0.01017044, count_min = 3,
  count_max = 3, match_count_min = 3, sim_annealing = FALSE,
  random_seed = FALSE, lopside_thresh = 0.65,
  drop_very_lopsided_votes = FALSE)
