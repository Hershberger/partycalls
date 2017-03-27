library(partycalls)
library(ggplot2)
library(gridExtra)


# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]


# formulas necessary for analysis
hou_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_votepct + south + votepct + female + afam + latino +
  seniority + freshman + bestgrosswart + leader +
  power + chair
sen_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share +
  female + afam + latino + up_for_reelection +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair


# Make Figure 1
hou_dem_base_plot <- ggplot(new_whoheeds13[dem == 1, ],
  aes(ideological_extremism, pfrate100)) + ylim(0, 100) +
  geom_point(color = "blue2", shape = 16, alpha = .25) +
  geom_smooth(method=loess, color = "gray30") +
  ggtitle("Democrats") +
  labs(x = "", y = "Baseline Party Rate") + theme_classic()

hou_rep_base_plot <- ggplot(new_whoheeds13[dem == 0 & ideological_extremism < 5, ],
  aes(ideological_extremism, pfrate100)) + ylim(0, 100) +
  geom_point(color = "red2", shape = 16, alpha = .25) +
  geom_smooth(method=loess, color = "gray30") +
  ggtitle("Republicans") +
  labs(x = "", y = "") + theme_classic()

hou_dem_call_plot <- ggplot(new_whoheeds13[dem == 1, ],
  aes(ideological_extremism, pirate100)) + ylim(0, 100) +
  geom_point(color = "blue2", shape = 16, alpha = .25) +
  geom_smooth(method=loess, color = "gray30") +
  ggtitle("") +
  labs(x = "Ideological Extremism", y = "Party Call Rate") + theme_classic()

hou_rep_call_plot <- ggplot(new_whoheeds13[dem == 0 & ideological_extremism < 5, ],
  aes(ideological_extremism, pirate100)) + ylim(0, 100) +
  geom_point(color = "red2", shape = 16, alpha = .25) +
  geom_smooth(method=loess, color = "gray30") +
  labs(x = "Ideological Extremism", y = "") + theme_classic()

fig1 <- arrangeGrob(hou_dem_base_plot, hou_rep_base_plot, hou_dem_call_plot,
  hou_rep_call_plot, ncol = 2, nrow = 2)
ggsave("plots/house_responsiveness_plot.pdf", fig1)


# Make Figure 2
sen_dem_base_plot <- ggplot(senate_data[caucus == "Democrat",],
  aes(ideological_extremism, pfrate100)) + ylim(0, 100) +
  geom_point(color = "blue2", shape = 16, alpha = .5) +
  geom_smooth(method = loess, color = "gray30") +
  ggtitle("Democrats") +
  labs(x = "", y = "Baseline Rate") +
  theme_classic()

sen_rep_base_plot <- ggplot(senate_data[caucus == "Republican",],
  aes(ideological_extremism, pfrate100)) + ylim(0, 100) +
  geom_point(color = "red2", shape = 16, alpha = .5) +
  geom_smooth(method = loess, color = "gray30") +
  ggtitle("Republicans") +
  labs(x = "", y = "") +
  theme_classic()

sen_dem_call_plot <- ggplot(senate_data[caucus == "Democrat",],
  aes(ideological_extremism, pirate100)) + ylim(0, 100) +
  geom_point(color = "blue2", shape = 16, alpha = .5) +
  geom_smooth(method = loess, color = "gray30") +
  labs(x = "Ideological Extremism", y = "Party Call Rate") +
  theme_classic()

sen_rep_call_plot <- ggplot(senate_data[caucus == "Republican",],
  aes(ideological_extremism, pirate100)) + ylim(0, 100) +
  geom_point(color = "red2", shape = 16, alpha = .5) +
  geom_smooth(method = loess, color = "gray30") +
  labs(x = "Ideological Extremism", y = "") +
  theme_classic()

fig2 <- arrangeGrob(sen_dem_base_plot, sen_rep_base_plot, sen_dem_call_plot,
  sen_rep_call_plot, ncol = 2, nrow = 2)
ggsave("plots/senate_responsiveness_plot.pdf", fig2)


# Make Table 1
texreg::texreg(list(
  lm(hou_extremism, new_whoheeds13[dem == 1]),
  lm(hou_extremism, new_whoheeds13[dem == 0]),
  lm(hou_extremism, new_whoheeds13[majority == 1]),
  lm(hou_extremism, new_whoheeds13[majority == 0])
), reorder.coef = c(2:3, 6, 4, 5, 7:15, 1),
  digits = 3)


# Make Table 2
texreg::texreg(list(lm(sen_extremism, senate_data[caucus == "Democrat"]),
  lm(sen_extremism, senate_data[caucus == "Republican"]),
  lm(sen_extremism, senate_data[maj == 1]),
  lm(sen_extremism, senate_data[maj == 0])),
  reorder.coef = c(2:3, 10, 6, 4, 5, 7:9, 11:17, 1),
  digits = 3)
