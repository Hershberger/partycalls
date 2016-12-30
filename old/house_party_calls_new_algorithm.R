library(partycalls)
library(data.table)
library(ggplot2)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

# what other things do we want to possibly change?
# 2. vote-level regression
#   a. change use_brglm = FALSE
#   b. change return_pvals = TRUE
#   c. set pval_threshold = .01
# 4. change stopping rule with an increase to count_min, an increase to
#   count_max, an increase to match_count_min and switch to
#   use_new_match_check = TRUE
# 1. change initial vote seeding to random_seed = TRUE
# 3. add simulated annealing with  sim_annealing = TRUE
# 5. change gray vote classification with an increase in n_iterations_for_coding
#
# set.seed(1975242355)
# code_party_calls_by_congress_number <- function(congress_number)
# {
#   cat("**** working on house", congress_number, "\n")
#   rc <- get(paste0("h", sprintf("%03.f", congress_number)))
#   rc <- code_party_calls(
#     rc,
#     pval_threshold = 0.01,
#     tval_threshold = 2.32,
#     count_min = 10,
#     count_max = 50,
#     match_count_min = 5,
#     sim_annealing = FALSE,
#     random_seed = FALSE,
#     lopside_thresh = 0.65,
#     drop_very_lopsided_votes = TRUE,
#     return_pvals = TRUE,
#     n_iterations_for_coding = 5,
#     use_new_match_check = FALSE,
#     type = "brglm")
# }
# house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
# names(house_party_calls) <- paste0("hou", 93:109)
# save(house_party_calls,
#   file = "inst/extdata/house_party_calls_new_stopping_rule.RData")

# try to get as close a match as possible using only the 93rd
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress == 93,
  .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

congress_number <- 93
rc <- get(paste0("h", sprintf("%03.f", congress_number)))
replication_93 <- code_party_calls(
  rc,
  pval_threshold = 0.01,
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
  type = "brglm")
replicate_partycalls <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", replication_93$source),
  voteno = replication_93$party_call_coding$voteno,
  replicate_coding = replication_93$party_call_coding$coding,
  replicate_tvals = rowMeans(do.call(cbind,
    tail(replication_93$record_of_tvals, 5))))

merge(old_partycalls, replicate_partycalls, by = c("congress", "voteno"))[,
  table(old_coding, replicate_coding)]

# switch away from brglm
coding_01 <- code_party_calls(
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
  use_new_match_check = TRUE,
  type = "glm")
partycalls_01 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", coding_01$source),
  voteno = coding_01$party_call_coding$voteno,
  new_coding = coding_01$party_call_coding$coding,
  new_tvals = rowMeans(do.call(cbind,
    tail(coding_01$record_of_tvals, 5))))

X <- merge(replicate_partycalls, partycalls_01, by = c("congress", "voteno"))
X[, table(replicate_coding, new_coding)]
ggplot(X, aes(replicate_tvals, new_tvals,
  color = factor(new_coding == replicate_coding))) + geom_point(alpha = .25) +
  theme_bw() + theme(legend.position = "none") + coord_equal() +
  xlim(-15, 15) + ylim(-15, 15)


# switch away from brglm
coding_02 <- code_party_calls(
  rc,
  pval_threshold =  0.01,
  tval_threshold = 2.32,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = FALSE,
  random_seed = TRUE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = FALSE,
  n_iterations_for_coding = 5,
  use_new_match_check = TRUE,
  type = "brglm")
partycalls_02 <- data.table(
  congress = gsub("[A-Za-z:/\\.]", "", coding_02$source),
  voteno = coding_02$party_call_coding$voteno,
  new_coding = coding_02$party_call_coding$coding,
  new_tvals = rowMeans(do.call(cbind,
    tail(coding_02$record_of_tvals, 5))))

X <- merge(replicate_partycalls, partycalls_02, by = c("congress", "voteno"))
X[, table(replicate_coding, new_coding)]
ggplot(X, aes(replicate_tvals, new_tvals,
  color = factor(new_coding == replicate_coding))) + geom_point(alpha = .25) +
  theme_bw() + theme(legend.position = "none") + coord_equal() +
  xlim(-15, 15) + ylim(-15, 15)



# switch away from brglm
coding_03 <- code_party_calls(
  h102,
  pval_threshold =  0.01,
  tval_threshold = 2.32,
  count_min = 10,
  count_max = 50,
  match_count_min = 5,
  sim_annealing = TRUE,
  random_seed = TRUE,
  lopside_thresh = 0.65,
  drop_very_lopsided_votes = TRUE,
  return_pvals = FALSE,
  n_iterations_for_coding = 5,
  use_new_match_check = TRUE,
  type = "brglm")
