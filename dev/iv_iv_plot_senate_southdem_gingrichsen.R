library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_p_05.RData")

senate_data <- senate_data[drop == 0, ]

senate_data[, south := as.factor(south)]
senate_data[, majority := as.factor(maj)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]

# save as plots/senate_p_05_dem_iv_iv_2_south.pdf
ggplot(senate_dem, aes(ideological_extremism, pfrate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_p_05_dem_iv_iv_2_majority.pdf
ggplot(senate_dem, aes(ideological_extremism, pfrate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "0.75", "1"),
    values = c("blue2", "gray55", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_p_05_rep_iv_iv_2_gingrich.pdf
ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
      values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_p_05_rep_iv_iv_2_majority.pdf
ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "0.75", "1"),
    values = c("red2", "gray55", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_p_05_rep_iv_iv_2_south.pdf
ggplot(senate_rep, aes(ideological_extremism, pfrate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
