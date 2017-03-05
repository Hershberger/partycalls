library(partycalls)
library(ggplot2)

load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]


ggplot(senate_data, aes(pres_vote_share, pres_dem_vote_share), color = caucus) +
  geom_point() +
  theme_bw()
ggsave("plots/pres_vote_check_all.pdf")

dev.off()

ggplot(senate_dem, aes(pres_vote_share, pres_dem_vote_share), color = caucus) +
  geom_point() +
  theme_bw()
ggsave("plots/pres_vote_check_dem.pdf")

dev.off()

ggplot(senate_rep, aes(pres_vote_share, pres_dem_vote_share), color = caucus) +
  geom_point() +
  theme_bw()
ggsave("plots/pres_vote_check_rep.pdf")

dev.off()
