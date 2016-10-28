library(partycalls)
library(data.table)
library(ggplot2)
options(stringsAsFactors = FALSE)

congress_number <- 109

load("inst/extdata/houKHfiles001-111.rdata")
set.seed(1975242355)

# try to get as close a match as possible using only the 93rd
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress == congress_number,
  .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

old_partycalls[, mean(old_coding == "gray")]

rc <- get(paste0("h", sprintf("%03.f", congress_number)))
replication <- code_party_calls(
  rc,
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
replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding = replication$party_call_coding$coding,
  replicate_pvals = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))

test1_gray <- replicate_partycalls[, mean(replicate_coding == "gray")]

test1_crosstab <- merge(old_partycalls, replicate_partycalls, by =
    c("voteno", "congress"))[, table(old_coding, replicate_coding)]


library(partycalls)
library(data.table)
library(ggplot2)
options(stringsAsFactors = FALSE)

congress_number <- 109

load("inst/extdata/houKHfiles001-111.rdata")
set.seed(554242791)

# try to get as close a match as possible using only the 93rd
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress == congress_number,
  .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

old_partycalls[, mean(old_coding == "gray")]

rc <- get(paste0("h", sprintf("%03.f", congress_number)))
replication <- code_party_calls(
  rc,
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
replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication$source),
  voteno = replication$party_call_coding$voteno,
  replicate_coding = replication$party_call_coding$coding,
  replicate_pvals = rowMeans(do.call(cbind,
    tail(replication$record_of_pvals, 5))))

replicate_partycalls[, mean(replicate_coding == "gray")]

merge(old_partycalls, replicate_partycalls, by = c("voteno", "congress"))[,
  table(old_coding, replicate_coding)]
