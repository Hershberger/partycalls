library(partycalls)

load("test_data/house_party_calls_lm.RData")

vote_coding_house_lm <- rbindlist(lapply(93:112, function(i) {
  cat("\r working on congress", i)
  out <- check_signs(house_party_calls[[paste0("hou", i)]])
  X <- house_party_calls[[paste0("hou", i)]]$voteMargins
  cbind(out, data.table(
    congress = i,
    party_call = house_party_calls[[paste0("hou", i)]]$party_call_coding$coding,
    close_vote = ifelse(
      X[, 4] / (X[, 1] + X[, 2]) <= .35,
      "lop", "close")
  ))
}))

vote_coding_house_lm[, mean(party_call == "gray")] #0.03337523

vote_coding_house_lm[party_call != "gray", table(close_vote, party_call)]
# party_call
# close_vote noncall party call
# close    1091       9305
# lop      6122       4248

vote_coding_house_lm[party_call != "gray",
  cor(close_vote == "close", party_call == "party call")]  # .51
vote_coding_house_lm[party_call == "party call",
  mean(party_coef == ideal_coef)] -
  vote_coding_house_lm[party_call == "noncall",
    mean(party_coef == ideal_coef)] # 0.25834592

vote_coding_house_lm[party_call %in% c("party call", "noncall"),
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     8127     2888
# positive     3394     6357

vote_coding_house_lm[party_call == "party call",
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     6166     1042
# positive     1312     5033
vote_coding_house_lm[party_call == "noncall",
  table(party_coef, ideal_coef)]
# ideal_coef
# party_coef negative positive
# negative     1961     1846
# positive     2082     1324
