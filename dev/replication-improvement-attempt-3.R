# library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
source("dev/functions-improvement-attempt-2.R")
load("inst/extdata/houKHfiles001-111.rdata")
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress < 110, .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]
whoheeds13 <- readstata13::read.dta13(
  "inst/extdata/who-heeds-replication-archive.dta")
setDT(whoheeds13)
old_ideal_points <- whoheeds13[,
  .(congress, icpsr, dem, old_ideal_point = ideal_partyfree)]

set.seed(1584882915)

new_coding <- code_party_calls(
  h109,
  pval_threshold = 0.01,
  count_min = 15,
  count_max = 150,
  match_count_min = 5,
  sim_annealing = TRUE,
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
  randomly_reassign_flip_flop_votes_from_noncalls = FALSE)

replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", new_coding$source),
  voteno = new_coding$party_call_coding$voteno,
  replicate_coding = new_coding$party_call_coding$coding,
  replicate_pvals = rowMeans(do.call(cbind,
    tail(new_coding$record_of_pvals, 5))))

new_ideal_points <- {
  rc <- make_member_year_data(109, list(hou109 = new_coding))
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsr = icpsrLegis, new_ideal_point = pf_ideal)]
}
merged_ideal_points <- merge(old_ideal_points, new_ideal_points,
  by = c("congress", "icpsr"), all = TRUE)

# report results:

replicate_partycalls[, mean(replicate_coding == "gray")]

merge(old_partycalls, replicate_partycalls, by =
    c("voteno", "congress"))[, table(old_coding, replicate_coding)]

merged_ideal_points[, cor(old_ideal_point, new_ideal_point, use = "p")]

plot(merged_ideal_points$old_ideal_point, merged_ideal_points$new_ideal_point)
