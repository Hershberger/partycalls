library(partycalls)
library(ggplot2)

theme_set(theme_classic())

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]

new_whoheeds13[, gingrich_leadership := 0] # before
new_whoheeds13[congress %in% c(101:105), gingrich_leadership := 1] # during
new_whoheeds13[congress %in% c(106:112), gingrich_leadership := 2] # after

new_whoheeds13[, congress := as.factor(congress)]
new_whoheeds13[, south := as.factor(south)]

senate_data[, congress := as.factor(congress)]
senate_data[, south := as.factor(south)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_data[congress == 107 & caucus == "Democrat", maj := 1]
senate_data[congress == 107 & caucus == "Republican", maj := 0]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]

house_dem <- new_whoheeds13[dem == 1, ]
house_rep <- new_whoheeds13[dem == 0, ]

# House Democrats/Southern Democrats
ggplot(house_dem, aes(ideological_extremism, pfrate100, color = south)) +
  geom_point(shape = 16, alpha = .5) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/house_dem_iv-iv_south_confint.pdf")

dev.off()

ggplot(house_dem, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16, alpha = .5) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/house_dem_iv-dv_south_confint.pdf")

dev.off()


# House Republicans and Gingrich
ggplot(house_rep[gingrich_leadership == 0, ], aes(ideological_extremism, pfrate100,
  color = congress)) +
  geom_point(shape = 16, alpha = .5) +
  xlim(-1.5, 4) +
  geom_smooth(method=loess)
ggsave("plots/house_rep_iv-iv_before_gingrich_confint.pdf")

dev.off()

ggplot(house_rep[gingrich_leadership == 0, ], aes(ideological_extremism, pirate100,
  color = congress)) +
  geom_point(shape = 16, alpha = .5) +
  xlim(-1.5, 4) +
  geom_smooth(method=loess)
ggsave("plots/house_rep_iv-dv_before_gingrich_confint.pdf")

dev.off()


ggplot(house_rep[gingrich_leadership == 1, ], aes(ideological_extremism, pfrate100,
  color = congress)) +
  geom_point(shape = 16, alpha = .5) +
  xlim(-1, 4) +
  geom_smooth(method=loess)
ggsave("plots/house_rep_iv-iv_during_gingrich_confint.pdf")

dev.off()

ggplot(house_rep[gingrich_leadership == 1, ], aes(ideological_extremism, pirate100,
  color = congress)) +
  geom_point(shape = 16, alpha = .5) +
  xlim(-1, 4) +
  geom_smooth(method=loess)
ggsave("plots/house_rep_iv-dv_during_gingrich_confint.pdf")

dev.off()


ggplot(house_rep[gingrich_leadership == 2, ], aes(ideological_extremism, pfrate100,
  color = congress)) +
  geom_point(shape = 16, alpha = .5) +
  xlim(-1, 4) +
  geom_smooth(method=loess)
ggsave("plots/house_rep_iv-iv_after_gingrich_confint.pdf")

dev.off()

ggplot(house_rep[gingrich_leadership == 2, ], aes(ideological_extremism, pirate100,
  color = congress)) +
  geom_point(shape = 16, alpha = .5) +
  xlim(-1, 4) +
  geom_smooth(method=loess)
ggsave("plots/house_rep_iv-dv_after_gingrich_confint.pdf")

dev.off()


# Senate Democrats/Southern Democrats
ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = south)) +
  geom_point(shape = 16, alpha = .65) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_dem_iv-iv_south_confint.pdf")

dev.off()

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16, alpha = .65) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_dem_iv-dv_south_confint.pdf")

dev.off()


# Senate Republicans/Gingrich Senators
ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = gingrich_senator)) +
  geom_point(shape = 16, alpha = .65) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_rep_iv-iv_gingrich_confint.pdf")

dev.off()

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = gingrich_senator)) +
  geom_point(shape = 16, alpha = .65) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_rep_iv-dv_gingrich_confint.pdf")

dev.off()
