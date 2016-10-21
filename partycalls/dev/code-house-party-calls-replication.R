library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

load("inst/extdata/houKHfiles001-111.rdata")

set.seed(1584882915)
code_party_calls_by_congress_number <- function(congress_number)
{
  cat("**** working on house", congress_number, "\n")
  rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  rc <- code_party_calls(rc, tval_threshold = 2.32,
    count_min = 10,
    count_max = 50, match_count_min = 5, sim_annealing = FALSE,
    random_seed = FALSE, lopside_thresh = 0.65,
    drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
    n_iterations_for_coding = 5, use_new_match_check = FALSE)
}
house_party_calls <- lapply(93:109, code_party_calls_by_congress_number)
names(house_party_calls) <- paste0("hou", 93:109)
save(house_party_calls, file = "inst/extdata/house_party_calls.RData")


new_partycalls <- rbindlist(lapply(house_party_calls, function(x) data.table(
    congress = gsub("[A-Za-z:/\\.]", "", x$source),
    voteno = x$party_call_coding$voteno,
    new_coding = x$party_call_coding$coding)))
load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress < 110, .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

merged_votes <- merge(old_partycalls, new_partycalls,
  by = c("congress", "voteno"), all = TRUE)
tab1 <- merged_votes[, table(old_coding, new_coding)]
tab2 <- merged_votes[congress != 104, table(old_coding, new_coding)]
chisq.test(tab1)
chisq.test(tab2)
merged_votes[, chisq.test(table(old_coding, new_coding))$stat, congress]
diag_prop <- function(x) sum(diag(x)) / sum(x)
merged_votes[congress != 104, diag_prop(table(old_coding, new_coding))]
merged_votes[congress != 104, diag_prop(table(old_coding, new_coding)), congress]
merged_votes[congress == 102, table(old_coding, new_coding)]
merged_votes[congress == 103, table(old_coding, new_coding)]
merged_votes[congress == 109, table(old_coding, new_coding)]
merged_votes[congress == 96, table(old_coding, new_coding)]
merged_votes[congress == 98, table(old_coding, new_coding)]
