library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]
senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]

senate_data[, south := as.factor(south)]
senate_data[, majority := as.factor(maj)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]


ggplot(senate_dem, aes(ideological_extremism, pfrate100, color = south)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray5")) +
  geom_smooth(method=loess)
ggsave("plots/senate_dem_iv-iv_south_confint.pdf")

dev.off()

ggplot(senate_dem, aes(ideological_extremism, pfrate100, color = majority)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray5")) +
  geom_smooth(method=loess)
ggsave("plots/senate_dem_iv-iv_majority_confint.pdf")

dev.off()

ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = gingrich_senator)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
      values = c("red2", "gray5")) +
  geom_smooth(method=loess)
ggsave("plots/senate_rep_iv-iv_gingrich_confint.pdf")

dev.off()

ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = majority)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray5")) +
  geom_smooth(method=loess)
ggsave("plots/senate_rep_iv-iv_majority_confint.pdf")

dev.off()
