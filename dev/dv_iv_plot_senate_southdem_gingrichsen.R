library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_p_05.RData")

senate_data <- senate_data[drop == 0, ]

senate_data[, south := as.factor(south)]
senate_data[, majority := as.factor(maj)]
senate_data[, gingrich_senator := as.factor(gingrich_senator)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]

# bivariate regressions
dem_lm1 <- lm(pirate100 ~ pfrate100, data = senate_dem)
dem_lm2 <- lm(pirate100 ~ ideological_extremism, data = senate_dem)
rep_lm1 <- lm(pirate100 ~ pfrate100, data = senate_rep)
rep_lm2 <- lm(pirate100 ~ ideological_extremism, data = senate_rep)

# multivariate regressions
dem_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = senate_dem)
rep_lm3 <- lm(pirate100 ~ pfrate100 + ideological_extremism, data = senate_rep)

# # plots
# pdf(file="plots/senate-emIRT_dem_iv-dv_1.pdf", ## RENAME
# width = 6, height = 4, family = "Times")
plot(senate_dem$pfrate100, senate_dem$pirate100, type = "p",
  xlab = "Responsiveness to Noncalls", ylab = "Responsiveness to Party Calls")
abline(dem_lm1)
# dev.off()

ggplot(senate_dem, aes(pfrate100, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_dem, aes(pfrate100, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

# pdf(file="plots/senate-emIRT_dem_iv-dv_2.pdf", ## RENAME
#   width = 6, height = 4, family = "Times")
plot(senate_dem$ideological_extremism, senate_dem$pirate100, type = "p",
  xlab = "Ideological Extremism", ylab = "Responsiveness to Party Calls")
abline(dem_lm2)
# dev.off()

ggplot(senate_dem, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_dem, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

ggplot(senate_dem, aes(ideological_extremism, pirate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "0.75", "1"),
    values = c("blue2", "gray55", "gray25")) +
  geom_smooth(method=loess, se=TRUE)


# pdf(file="plots/senate-emIRT_rep_iv-dv_1.pdf", ## RENAME
# width = 6, height = 4, family = "Times")
plot(senate_rep$pfrate100, senate_rep$pirate100, type = "p",
  xlab = "Responsiveness to Noncalls", ylab = "Responsiveness to Party Calls")
abline(rep_lm1)
# dev.off()

ggplot(senate_rep, aes(pfrate100, pirate100, color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_rep, aes(pfrate100, pirate100, color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

# pdf(file="plots/senate-emIRT_rep_iv-dv_2.pdf", ## RENAME
# width = 6, height = 4, family = "Times")
plot(senate_rep$ideological_extremism, senate_rep$pirate100, type = "p",
  xlab = "Ideological Extremism", ylab = "Responsiveness to Party Calls")
abline(rep_lm2)
# dev.off()

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = gingrich_senator)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
      values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=TRUE)

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = majority)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "0.75", "1"),
    values = c("red2", "gray55", "gray25")) +
  geom_smooth(method=loess, se=TRUE)
