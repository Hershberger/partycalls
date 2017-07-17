library(partycalls)

load("test_data/senate_party_calls_lm.RData")

vote_coding_senate_lm <- rbindlist(lapply(93:112, function(i) {
  cat("\r working on congress", i)
  out <- check_signs(senate_party_calls[[paste0("sen", i)]])
  X <- senate_party_calls[[paste0("sen", i)]]$voteMargins
  cbind(out, data.table(
    congress = i,
    party_call = senate_party_calls[[paste0("sen", i)]]$party_call_coding$coding,
    close_vote = ifelse(
      X[, 4] / (X[, 1] + X[, 2]) <= .35,
      "lop", "close")
  ))
}))

vote_coding_senate_lm[, mean(party_call == "gray")] #0.004

vote_coding_senate_lm[party_call != "gray", table(close_vote, party_call)]
# party_call
# close_vote noncall party call
# close    1857       5228
# lop      4870       2068

vote_coding_senate_lm[party_call != "gray",
  cor(close_vote == "close", party_call == "party call")]  # .44
vote_coding_senate_lm[party_call == "party call",
  mean(party_coef == ideal_coef)] -
  vote_coding_senate_lm[party_call == "noncall",
    mean(party_coef == ideal_coef)] # 0.25834592

vote_coding_senate_lm[party_call %in% c("party call", "noncall"),
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     4581     2244
# positive     3188     4010

vote_coding_senate_lm[party_call == "party call",
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     2807      793
# positive     1129     2567

vote_coding_senate_lm[party_call == "noncall",
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     1774     1451
# positive     2059     1443
