library(senatePartyCalls)
library(ggplot2)

formula_1 <- responsiveness_party_calls ~ dist_from_floor_median +
  responsiveness_noncalls + vote_share * pres_vote_share +
  up_for_reelection + freshman + retiree + seniority +
  leader + chair + best_committee + power_committee +
  south13 + afam + female + latino

formula_2 <- responsiveness_party_calls ~  ideological_extremism +
  responsiveness_noncalls + vote_share * pres_vote_share +
  up_for_reelection + freshman + retiree + seniority +
  leader + chair + best_committee + power_committee +
  south13 + afam + female + latino

texreg::screenreg(list(
  lm(formula_1, senator_year_data[caucus == "Democrat" & drop == 0]),
  lm(formula_1, senator_year_data[caucus == "Republican" & drop == 0]),
  lm(formula_2, senator_year_data[caucus == "Democrat" & drop == 0]),
  lm(formula_2, senator_year_data[caucus == "Republican" & drop == 0])),
  reorder.coef = c(2, 19, 3:5, 18, 6:17, 1), digits = 3
  )

piecewise_lm <- function(formula, .SD)
{
  model <- lm(formula, .SD)
  summ <- summary(model)
  coefs <- summ$coef[3, ]
  as.list(coefs)
}

results <- cbind(
  CJ(party = c("D", "R"), model = c("ideological extremism",
    "distance from floor median"), congress = 93:112),
  rbind(
    senator_year_data[caucus == "Democrat" & drop == 0,
      piecewise_lm(formula_1, .SD), .(congress)],
    senator_year_data[caucus == "Democrat" & drop == 0,
      piecewise_lm(formula_2, .SD), .(congress)],
    senator_year_data[caucus == "Republican" & drop == 0,
      piecewise_lm(formula_1, .SD), .(congress)],
    senator_year_data[caucus == "Republican" & drop == 0,
      piecewise_lm(formula_2, .SD), .(congress)]
  ))
results[, q025 := Estimate + qnorm(.025) * `Std. Error`]
results[, q975 := Estimate + qnorm(.975) * `Std. Error`]
results[, q250 := Estimate + qnorm(.25) * `Std. Error`]
results[, q750 := Estimate + qnorm(.75) * `Std. Error`]

# 107th is an odd case because of Jeffords
results[, status := "majority"]
results[party == "R", status := "minority"]
results[party == "R" & congress %in% c(97:99, 104:106, 108:109),
  status := "majority"]
results[party == "D" & congress %in% c(97:99, 104:106, 108:109),
  status := "minority"]

ggplot(results[], aes(congress, Estimate,
  color = party)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 2) +
  geom_errorbar(aes(ymin = q250, ymax = q750), width = 0, size = 1.1) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0) +
  geom_point(size = 5, shape = 45) + facet_grid(status ~ model) + theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "white"),
    legend.position = "none") +
  ylab("") +
  scale_color_manual(values = c("D" = "blue", "R" = "red"))

ggsave("doc/spc-fig1.png", height = 3.25, width = 6.5)
# What do we learn from this?