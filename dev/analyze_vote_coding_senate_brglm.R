library(partycalls)

load("test_data/senate_party_calls_brglm.RData")

vote_coding_senate_brglm <- rbindlist(lapply(93:112, function(i) {
  cat("\r working on congress", i)
  out <- check_signs(senate_party_calls_brglm[[paste0("sen", i)]])
  X <- senate_party_calls_brglm[[paste0("sen", i)]]$voteMargins
  cbind(out, data.table(
    congress = i,
    party_call = senate_party_calls_brglm[[paste0("sen", i)]]$party_call_coding$coding,
    close_vote = ifelse(
      X[, 4] / (X[, 1] + X[, 2]) <= .35,
      "lop", "close")
  ))
}))

vote_coding_senate_brglm[, mean(party_call == "gray")] #0.03337523

vote_coding_senate_brglm[party_call != "gray", table(close_vote, party_call)]
# party_call
# close_vote noncall party call
# close    4745       2192
# lop      5425       1446

vote_coding_senate_brglm[party_call != "gray",
  cor(close_vote == "close", party_call == "party call")]  # .51
vote_coding_senate_brglm[party_call == "party call",
  mean(party_coef == ideal_coef)] -
  vote_coding_senate_brglm[party_call == "noncall",
    mean(party_coef == ideal_coef)] # 0.25834592


vote_coding_senate_brglm[party_call == "party call",
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative      605     1039
# positive     1376      618
vote_coding_senate_brglm[party_call == "noncall",
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     2874     2047
# positive     2816     2433
