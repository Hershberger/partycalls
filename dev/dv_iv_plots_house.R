library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/new_whoheeds13_emIRT_only.RData")

new_whoheeds13 <- new_whoheeds13[drop == 0, ]

new_whoheeds13[, south := as.factor(south)]
new_whoheeds13[, majority := as.factor(majority)]

house_dem <- new_whoheeds13[dem == 1, ]
house_rep <- new_whoheeds13[dem == 0, ]


# save as plots/house_dem_iv-dv_2_south.pdf
ggplot(house_dem, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/house_dem_iv-dv_2_majority.pdf
ggplot(house_dem, aes(ideological_extremism, pirate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/house_rep_iv-dv_2_majority.pdf
ggplot(house_rep, aes(ideological_extremism, pirate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/house_rep_iv-dv_2_south.pdf
ggplot(house_rep, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
