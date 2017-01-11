library(partycalls)

load("test_data/house_party_calls_emIRT_only.RData")
names(house_party_calls) <- paste0("hou", 93:112)

house_coding_record <- data.frame(row.names = 93:112)
house_coding_record$party_call_count <- sapply(93:112, function(x)
  length(get_party_calls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$noncall_count <- sapply(93:112, function(x)
  length(get_noncalls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$gray_vote_count <- sapply(93:112, function(x)
  length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))

sum(house_coding_record$party_call_count)
sum(house_coding_record$noncall_count)
sum(house_coding_record$gray_vote_count)
