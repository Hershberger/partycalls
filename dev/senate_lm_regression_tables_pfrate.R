library(partycalls)
library(texreg)
library(data.table)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]

f_extremism <- pfrate100 ~ ideological_extremism +
  pres_vote_share + south + vote_share +
  female + afam + latino + up_for_reelection +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair

texreg::screenreg(list(lm(f_extremism, senate_data[caucus == "Democrat" ]),
  lm(f_extremism, senate_data[caucus == "Republican"]),
  lm(f_extremism, senate_data[maj == 1]),
  lm(f_extremism, senate_data[maj == 0])),
  reorder.coef = c(2:16, 1),
  digits = 3)

texreg::texreg(list(lm(f_extremism, senate_data[caucus == "Democrat"]),
  lm(f_extremism, senate_data[caucus == "Republican"]),
  lm(f_extremism, senate_data[maj == 1]),
  lm(f_extremism, senate_data[maj == 0])),
  reorder.coef = c(2:16, 1),
  digits = 3)

f_extremism_dem <- pfrate100 ~ ideological_extremism +
  pres_vote_share + vote_share +
  female + afam + latino + up_for_reelection +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair

texreg::screenreg(list(
  lm(f_extremism_dem, senate_data[caucus == "Democrat" & south == 1]),
  lm(f_extremism_dem, senate_data[caucus == "Democrat" & south == 0]),
  lm(f_extremism, senate_data[caucus == "Republican" & gingrich_senator == 1]),
  lm(f_extremism, senate_data[caucus == "Republican" & gingrich_senator == 0])),
  reorder.coef = c(2:3, 15, 4:5, 6:14, 16, 1),
  digits = 3)

texreg::texreg(list(
  lm(f_extremism_dem, senate_data[caucus == "Democrat" & south == 1]),
  lm(f_extremism_dem, senate_data[caucus == "Democrat" & south == 0]),
  lm(f_extremism, senate_data[caucus == "Republican" & gingrich_senator == 1]),
  lm(f_extremism, senate_data[caucus == "Republican" & gingrich_senator == 0])),
  reorder.coef = c(2:3, 15, 4:5, 6:14, 16, 1),
  digits = 3)
