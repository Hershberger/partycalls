library(partycalls)

load("test_data/senate_party_calls_emIRT_only.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

senate_coding_record <- data.frame(row.names = 93:112)
senate_coding_record$congress <- 93:112
senate_coding_record$party_call_count <- sapply(93:112, function(x)
  length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))
senate_coding_record$noncall_count <- sapply(93:112, function(x)
  length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))
senate_coding_record$gray_vote_count <- sapply(93:112, function(x)
  length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))

sum(senate_coding_record$party_call_count)
sum(senate_coding_record$noncall_count)
sum(senate_coding_record$gray_vote_count)
