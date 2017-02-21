library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/new_whoheeds13_lm.RData")

new_whoheeds13 <- new_whoheeds13[drop == 0, ]

house_dem <- new_whoheeds13[dem == 1, ]
house_rep <- new_whoheeds13[dem == 0, ]
house_maj <- new_whoheeds13[majority == 1, ]
house_min <- new_whoheeds13[majority == 0, ]

# bivariate regressions by party
dem_lm1 <- lm(pirate100 ~ pfrate100, data = house_dem)
dem_lm2 <- lm(pirate100 ~ ideological_extremism, data = house_dem)
rep_lm1 <- lm(pirate100 ~ pfrate100, data = house_rep)
rep_lm2 <- lm(pirate100 ~ ideological_extremism, data = house_rep)

# multivariate regressions by party and majority/minority
dem_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = house_dem)
rep_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = house_rep)
maj_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = house_maj)
min_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = house_min)

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

