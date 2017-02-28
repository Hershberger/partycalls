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
senate_coding_record[, gray_votes := sapply(93:112, function(x)
  length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))]

senate_coding_record[, percent_party_calls :=
    party_calls / (party_calls + noncalls + gray_votes)]
senate_coding_record[, percent_noncalls :=
    noncalls / (party_calls + noncalls + gray_votes)]

ggplot(senate_coding_record, aes(congress, percent_party_calls)) +
  xlab("Congress") +
  ylab("Party Call Percent") +
  geom_point(color = "red") +
  geom_line() +
  theme_bw()
ggsave("plots/party_call_percent_plot_senate_lm.pdf")

dev.off()

ggplot(senate_coding_record, aes(congress, percent_noncalls)) +
  xlab("Congress") +
  ylab("Party Call Percent") +
  geom_point(color = "blue") +
  geom_line() +
  theme_bw()
ggsave("plots/non_call_percent_plot_senate_lm.pdf")

dev.off()
