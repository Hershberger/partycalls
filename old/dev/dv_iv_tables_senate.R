library(partycalls)

load("test_data/senate_data_p_05.RData")

senate_data <- senate_data[drop == 0, ]

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

texreg::screenreg(list(dem_lm3, rep_lm3, maj_lm3, min_lm3),
  reorder.coef = c(2:3, 1),
  digits = 3)
texreg::texreg(list(dem_lm3, rep_lm3, maj_lm3, min_lm3),
  reorder.coef = c(2:3, 1),
  digits = 3)
