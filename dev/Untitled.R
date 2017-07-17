library(partycalls)

load("test_data/senate_party_calls_lm.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

vote_coding <- rbindlist(lapply(93:112, function(i) {
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
vote_coding[party_call != "gray", table(close_vote, party_call)]

vote_coding[party_call != "gray",
  cor(close_vote == "close", party_call == "party call")]  # .44
vote_coding[party_call == "party call",
  cor(party_coef == "positive", ideal_coef == "positive")] # .48
vote_coding[party_call == "noncall",
  cor(party_coef == "positive", ideal_coef == "positive")] # -.04
