library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/new_whoheeds13_lm.RData")

new_whoheeds13 <- new_whoheeds13[drop == 0, ]

new_whoheeds13[, south := as.factor(south)]
new_whoheeds13[, majority := as.factor(majority)]

house_dem <- new_whoheeds13[dem == 1, ]
house_rep <- new_whoheeds13[dem == 0, ]
house_rep <- house_rep[ideological_extremism < 5, ]


ggplot(house_dem, aes(ideological_extremism, pfrate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_lm_dem_iv-iv_south.pdf")

dev.off()

ggplot(house_dem, aes(ideological_extremism, pfrate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_lm_dem_iv-iv_majority.pdf")

dev.off()

ggplot(house_rep, aes(ideological_extremism, pfrate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
ggsave("plots/house_lm_rep_iv-iv_majority.pdf")

dev.off()

# # save as plots/house_lm_rep_iv-iv_south.pdf
# ggplot(house_rep, aes(ideological_extremism, pfrate100, color = south)) +
#   geom_point(shape = 16) +
#   scale_color_manual(breaks = c("0", "1"),
#     values = c("red2", "gray25")) +
#   geom_smooth(method=loess, se=FALSE)
