library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]

senate_data[, south := as.factor(south)]
senate_data[, majority := as.factor(maj)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]
senate_maj <- senate_data[maj == 1, ]
senate_min <- senate_data[maj == 0, ]

# bivariate regressions by party
dem_lm1 <- lm(pirate100 ~ pfrate100, data = senate_dem)
dem_lm2 <- lm(pirate100 ~ ideological_extremism, data = senate_dem)
rep_lm1 <- lm(pirate100 ~ pfrate100, data = senate_rep)
rep_lm2 <- lm(pirate100 ~ ideological_extremism, data = senate_rep)

# multivariate regressions by party and majority/minority
dem_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = senate_dem)
rep_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = senate_rep)
maj_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = senate_maj)
min_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = senate_min)

# tables
texreg::screenreg(list(dem_lm1, dem_lm2, rep_lm1, rep_lm2),
  reorder.coef = c(2, 3, 1))
texreg::texreg(list(dem_lm1, dem_lm2, rep_lm1, rep_lm2),
  reorder.coef = c(2, 3, 1))

texreg::screenreg(list(dem_lm3, rep_lm3, maj_lm3, min_lm3),
  reorder.coef = c(2:3, 1),
  digits = 3)
texreg::texreg(list(dem_lm3, rep_lm3, maj_lm3, min_lm3),
  reorder.coef = c(2:3, 1),
  digits = 3)

# save as plots/senate_lm_dem_iv-dv_south.pdf
ggplot(senate_dem, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_lm_dem_iv-dv_majority.pdf
ggplot(senate_dem, aes(ideological_extremism, pirate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "0.75", "1"),
    values = c("blue2", "gray55", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_lm_rep_iv-dv_gingrich.pdf
ggplot(senate_rep, aes(ideological_extremism, pirate100, color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_lm_rep_iv-dv_majority.pdf
ggplot(senate_rep, aes(ideological_extremism, pirate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "0.75", "1"),
    values = c("red2", "gray55", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/senate_lm_rep_iv-dv_south.pdf
ggplot(senate_rep, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
