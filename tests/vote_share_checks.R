library(partycalls)
library(ggplot2)

load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]

# make sure all have vote_share > .5
senate_data[vote_share < .5, .(congress, mc, icpsrLegis)]

# make sure none have vote_share > 1
senate_data[vote_share > 1, .(congress, mc, icpsrLegis)]

# check those who have vote_share = 1
senate_data[vote_share == 1, .(congress, mc, icpsrLegis)]

# check same state mcs pres_vote_share
senate_data[stabb == "OH", .(caucus, congress, mc, pres_vote_share)]
senate_data[stabb == "TX", .(caucus, congress, mc, pres_vote_share)]
senate_data[stabb == "NY", .(caucus, congress, mc, pres_vote_share)]

ggplot(senate_data, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()
