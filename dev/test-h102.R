library(partycalls)
library(data.table)
library(ggplot2)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

# try to get as close a match as possible using only the 102nd
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress == 102,
  .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

congress_number <- 102
rc <- get(paste0("h", sprintf("%03.f", congress_number)))
replication_102 <- code_party_calls(
  rc,
  pval_threshold =  0.01,
  tval_threshold = 2.32,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = FALSE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = FALSE,
  n_iterations_for_coding = 5,
  use_new_match_check = FALSE,
  type = "brglm",
  use_classification_distance = TRUE)
replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication_102$source),
  voteno = replication_102$party_call_coding$voteno,
  replicate_coding = replication_102$party_call_coding$coding,
  replicate_tvals = rowMeans(do.call(cbind,
    tail(replication_102$record_of_tvals, 5))))

merge(old_partycalls, replicate_partycalls, by = c("congress", "voteno"))[,
  table(old_coding, replicate_coding)]
