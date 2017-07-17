library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]

new_whoheeds13[, south := as.factor(south)]

house_dem_maj <- new_whoheeds13[dem == 1 & majority == 1, ]
house_dem_min <- new_whoheeds13[dem == 1 & majority == 0, ]
house_rep_maj <- new_whoheeds13[dem == 0 & majority == 1, ]
house_rep_min <- new_whoheeds13[dem == 0 & majority == 0, ]

# remove outliers
house_rep_maj <- house_rep_maj[ideological_extremism < 5, ]


ggplot(house_dem_maj, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_iv_dv_dem_maj.pdf")

dev.off()


ggplot(house_dem_min, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_iv_dv_dem_min.pdf")

dev.off()


ggplot(house_rep_maj, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_iv_dv_rep_maj.pdf")

dev.off()


ggplot(house_rep_min, aes(ideological_extremism, pirate100)) +
  geom_point(shape = 16) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_iv_dv_rep_min.pdf")

dev.off()


ggplot(house_dem_maj, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16, alpha = 0.5) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray5")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_iv_dv_dem_maj_south.pdf")

dev.off()

ggplot(house_dem_min, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16, alpha = 0.75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray5")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_iv_dv_dem_min_south.pdf")

dev.off()
