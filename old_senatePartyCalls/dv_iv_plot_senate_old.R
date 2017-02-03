library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("old_senatePartyCalls/old_data/senator_year_data.rda")

senate_data <- senator_year_data[drop == 0, ]

senate_data[, south := as.factor(south13)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_dem <- senate_dem[ideological_extremism < 0,] # a few weird obs in this
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

# multivariate regressions
dem_lm3 <- lm(responsiveness_party_calls ~ responsiveness_noncalls +
    ideological_extremism, data = senate_dem)
rep_lm3 <- lm(responsiveness_party_calls ~ responsiveness_noncalls +
    ideological_extremism, data = senate_rep)

# # plots
# pdf(file="plots/senate-emIRT_dem_iv-dv_1.pdf", ## RENAME
# width = 6, height = 4, family = "Times")
plot(senate_dem$responsiveness_noncalls, senate_dem$responsiveness_party_calls, type = "p",
  xlab = "Responsiveness to Noncalls", ylab = "Responsiveness to Party Calls")
abline(dem_lm1)
# dev.off()

ggplot(senate_dem, aes(responsiveness_noncalls, responsiveness_party_calls,
  color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_dem, aes(responsiveness_noncalls, responsiveness_party_calls,
  color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

# pdf(file="plots/senate-emIRT_dem_iv-dv_2.pdf", ## RENAME
#   width = 6, height = 4, family = "Times")
plot(senate_dem$ideological_extremism, senate_dem$responsiveness_party_calls,
  type = "p",
  xlab = "Ideological Extremism", ylab = "Responsiveness to Party Calls")
abline(dem_lm2)
# dev.off()

ggplot(senate_dem, aes(ideological_extremism, responsiveness_party_calls,
  color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_dem, aes(ideological_extremism, responsiveness_party_calls,
  color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)


# pdf(file="plots/senate-emIRT_rep_iv-dv_1.pdf", ## RENAME
# width = 6, height = 4, family = "Times")
plot(senate_rep$responsiveness_noncalls, senate_rep$responsiveness_party_calls,
  type = "p",
  xlab = "Responsiveness to Noncalls", ylab = "Responsiveness to Party Calls")
abline(rep_lm1)
# dev.off()

ggplot(senate_rep, aes(responsiveness_noncalls, responsiveness_party_calls,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_rep, aes(responsiveness_noncalls, responsiveness_party_calls,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

# pdf(file="plots/senate-emIRT_rep_iv-dv_2.pdf", ## RENAME
# width = 6, height = 4, family = "Times")
plot(senate_rep$ideological_extremism, senate_rep$responsiveness_party_calls,
  type = "p",
  xlab = "Ideological Extremism", ylab = "Responsiveness to Party Calls")
abline(rep_lm2)
# dev.off()

ggplot(senate_rep, aes(ideological_extremism, responsiveness_party_calls,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_rep, aes(ideological_extremism, responsiveness_party_calls,
  color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

ggplot(senate_rep, aes(ideological_extremism, responsiveness_party_calls,
  color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

############################################################
## identify command will give names for individual points ##
############################################################
