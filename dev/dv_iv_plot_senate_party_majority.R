library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]

senate_data[, south := as.factor(south)]
senate_data[, majority := as.factor(maj)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem_maj <- senate_data[caucus == "Democrat" & maj > 0.5, ]
senate_dem_min <- senate_data[caucus == "Democrat" & maj < 0.5, ]
senate_rep_maj <- senate_data[caucus == "Republican" & maj > 0.5, ]
senate_rep_min <- senate_data[caucus == "Republican" & maj < 0.5, ]

# save as plots/senate_lm_dem_iv-dv_south.pdf
ggplot(senate_dem_maj, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_dem_maj.pdf")

dev.off()

# save as plots/senate_lm_dem_iv-dv_majority.pdf
ggplot(senate_dem_min, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_dem_min.pdf")

dev.off()

# save as plots/senate_lm_rep_iv-dv_gingrich.pdf
ggplot(senate_rep_maj, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_rep_maj.pdf")

dev.off()

# save as plots/senate_lm_rep_iv-dv_majority.pdf
ggplot(senate_rep_min, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/senate_iv_dv_rep_min.pdf")

dev.off()
