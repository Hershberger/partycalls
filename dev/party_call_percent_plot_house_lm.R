library(partycalls)
library(ggplot2)

load("test_data/house_party_calls_lm.RData")
names(house_party_calls) <- paste0("hou", 93:112)

# table for all congresses
house_coding_record <- data.table(congress = 93:112)
house_coding_record$party_calls <- sapply(93:112, function(x)
  length(get_party_calls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$noncalls <- sapply(93:112, function(x)
  length(get_noncalls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$gray_votes <- sapply(93:112, function(x)
  length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))

house_coding_record[, percent_party_calls :=
    party_calls / (party_calls + noncalls + gray_votes)]
house_coding_record[, percent_noncalls :=
    noncalls / (party_calls + noncalls + gray_votes)]

ggplot(house_coding_record, aes(congress, percent_party_calls)) +
  xlab("Congress") +
  ylab("Party Call Percent") +
  #labs(title = "Percent of Party Calls by Congress") +
  geom_point(color = "red1") +
  geom_line() +
  theme_bw()
ggsave("plots/party_call_percent_plot_house_lm.pdf")

dev.off()

ggplot(house_coding_record, aes(congress, percent_noncalls)) +
  xlab("Congress") +
  ylab("Party Call Percent") +
  #labs(title = "Percent of Party Calls by Congress") +
  geom_point(color = "blue") +
  geom_line() +
  theme_bw()
ggsave("plots/non_call_percent_plot_house_lm.pdf")

dev.off()
