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

senate_data_nixon <- senate_data[congress =< 94, ]
ggplot(senate_data_nixon, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_carter <- senate_data[congress %in% c(95, 96), ]
ggplot(senate_data_carter, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_reagan1 <- senate_data[congress %in% c(97, 98), ]
ggplot(senate_data_reagan1, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_reagan2 <- senate_data[congress %in% c(99, 100), ]
ggplot(senate_data_reagan2, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_bush1 <- senate_data[congress %in% c(101, 102), ]
ggplot(senate_data_bush1, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_clinton1 <- senate_data[congress %in% c(103, 104), ]
ggplot(senate_data_clinton1, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_clinton2 <- senate_data[congress %in% c(105, 106), ]
ggplot(senate_data_clinton2, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_bush2 <- senate_data[congress %in% c(107, 108), ]
ggplot(senate_data_bush2, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_bush3 <- senate_data[congress %in% c(109, 110), ]
ggplot(senate_data_bush3, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()

senate_data_obama <- senate_data[congress %in% c(111, 112), ]
ggplot(senate_data_obama, aes(vote_share, pres_vote_share, color = caucus)) +
  geom_point() + geom_smooth()






