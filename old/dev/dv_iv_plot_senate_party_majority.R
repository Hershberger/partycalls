library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]

senate_data[, south := as.factor(south)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem_maj <- senate_data[caucus == "Democrat" & maj > 0.5, ]
senate_dem_min <- senate_data[caucus == "Democrat" & maj < 0.5, ]
senate_rep_maj <- senate_data[caucus == "Republican" & maj > 0.5, ]
senate_rep_min <- senate_data[caucus == "Republican" & maj < 0.5, ]


ggplot(senate_dem_maj, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_dem_maj.pdf")

dev.off()


ggplot(senate_dem_min, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_dem_min.pdf")

dev.off()


ggplot(senate_rep_maj, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_rep_maj.pdf")

dev.off()


ggplot(senate_rep_min, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_rep_min.pdf")

dev.off()


ggplot(senate_dem_maj, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_dem_maj_south.pdf")

dev.off()


ggplot(senate_dem_min, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_dem_min_south.pdf")

dev.off()


ggplot(senate_rep_maj, aes(ideological_extremism, pirate100,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_rep_maj_gingrich.pdf")

dev.off()


ggplot(senate_rep_min, aes(ideological_extremism, pirate100,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_rep_min_gingrich.pdf")

dev.off()
