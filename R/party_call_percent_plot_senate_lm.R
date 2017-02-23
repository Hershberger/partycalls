library(partycalls)
library(ggplot2)

load("test_data/senate_party_calls_lm.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

# table for all congresses
senate_coding_record <- data.table(congress = 93:112)
senate_coding_record[, party_calls := sapply(93:112, function(x)
  length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, noncalls := sapply(93:112, function(x)
  length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))]

senate_coding_record[, percent_party_calls := party_calls / (party_calls + noncalls)]

plot(senate_coding_record$congress, senate_coding_record$percent_party_calls,
  type = "b")

ggplot(senate_coding_record, aes(congress, percent_party_calls)) +
  xlab("Congress") +
  ylab("Party Call Percent") +
  labs(title = "Percent of Party Calls by Congress") +
  geom_point(color = "red1") +
  geom_line() +
  theme_bw()
