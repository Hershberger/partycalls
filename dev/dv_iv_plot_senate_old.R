library(partycalls)

load("old_senatePartyCalls/old_data/senator_year_data.rda")

senate_data <- senator_year_data[drop == 0, ]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]

# bivariate regressions
dem_lm1 <- lm(responsiveness_party_calls ~ responsiveness_noncalls,
  data = senate_dem)
dem_lm2 <- lm(responsiveness_party_calls ~ ideological_extremism,
  data = senate_dem)
rep_lm1 <- lm(responsiveness_party_calls ~ responsiveness_noncalls,
  data = senate_rep)
rep_lm2 <- lm(responsiveness_party_calls ~ ideological_extremism,
  data = senate_rep)

# tables
texreg::screenreg(list(dem_lm1, dem_lm2, rep_lm1, rep_lm2),
  reorder.coef = c(2, 3, 1))
texreg::texreg(list(dem_lm1, dem_lm2, rep_lm1, rep_lm2),
  reorder.coef = c(2, 3, 1))

# # plots
# pdf(file="plots/senate-old_dem_iv-dv_1.pdf", ## RENAME
  # width = 6, height = 4, family = "Times")
plot(senate_dem$responsiveness_noncalls, senate_dem$responsiveness_party_calls,
  type = "p", xlab = "Responsiveness to Noncalls",
  ylab = "Responsiveness to Party Calls")
abline(dem_lm1)
# dev.off()

# pdf(file="plots/senate-old_dem_iv-dv_2.pdf", ## RENAME
#   width = 6, height = 4, family = "Times")
plot(senate_dem$ideological_extremism, senate_dem$responsiveness_party_calls,
  type = "p", xlab = "Ideological Extremism",
  ylab = "Responsiveness to Party Calls")
abline(dem_lm2)
# dev.off()

# pdf(file="plots/senate-old_rep_iv-dv_1.pdf", ## RENAME
  # width = 6, height = 4, family = "Times")
plot(senate_rep$responsiveness_noncalls, senate_rep$responsiveness_party_calls,
  type = "p", xlab = "Responsiveness to Noncalls",
  ylab = "Responsiveness to Party Calls")
abline(rep_lm1)
# dev.off()

# pdf(file="plots/senate-old_rep_iv-dv_2.pdf", ## RENAME
  # width = 6, height = 4, family = "Times")
plot(senate_rep$ideological_extremism, senate_rep$responsiveness_party_calls,
  type = "p", xlab = "Ideological Extremism",
  ylab = "Responsiveness to Party Calls")
abline(rep_lm2)
# dev.off()

senate_rep[, gingrich_senator := as.factor(gingrich_senator)]

ggplot(senate_rep, aes(ideological_extremism, responsiveness_party_calls,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)
