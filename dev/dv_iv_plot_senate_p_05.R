library(partycalls)

load("test_data/senate_data_p_05.RData")

senate_data <- senate_data[drop == 0, ]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]

# bivariate regressions
dem_lm1 <- lm(pirate100 ~ pfrate100, data = senate_dem)
dem_lm2 <- lm(pirate100 ~ ideological_extremism, data = senate_dem)
rep_lm1 <- lm(pirate100 ~ pfrate100, data = senate_rep)
rep_lm2 <- lm(pirate100 ~ ideological_extremism, data = senate_rep)

# tables
texreg::screenreg(list(dem_lm1, dem_lm2, rep_lm1, rep_lm2),
  reorder.coef = c(2, 3, 1))
texreg::texreg(list(dem_lm1, dem_lm2, rep_lm1, rep_lm2),
  reorder.coef = c(2, 3, 1))

# # plots
# pdf(file="plots/senate-p_05_dem_iv-dv_1.pdf", ## RENAME
  # width = 6, height = 4, family = "Times")
plot(senate_dem$pfrate100, senate_dem$pirate100, type = "p",
  xlab = "Responsiveness to Noncalls", ylab = "Responsiveness to Party Calls")
abline(dem_lm1)
# dev.off()

# pdf(file="plots/senate-p_05_dem_iv-dv_2.pdf", ## RENAME
#   width = 6, height = 4, family = "Times")
plot(senate_dem$ideological_extremism, senate_dem$pirate100, type = "p",
  xlab = "Ideological Extremism", ylab = "Responsiveness to Party Calls")
abline(dem_lm2)
# dev.off()

# pdf(file="plots/senate-p_05_rep_iv-dv_1.pdf", ## RENAME
  # width = 6, height = 4, family = "Times")
plot(senate_rep$pfrate100, senate_rep$pirate100, type = "p",
  xlab = "Responsiveness to Noncalls", ylab = "Responsiveness to Party Calls")
abline(rep_lm1)
# dev.off()

# pdf(file="plots/senate-p_05_rep_iv-dv_2.pdf", ## RENAME
  # width = 6, height = 4, family = "Times")
plot(senate_rep$ideological_extremism, senate_rep$pirate100, type = "p",
  xlab = "Ideological Extremism", ylab = "Responsiveness to Party Calls")
abline(rep_lm2)
# dev.off()
