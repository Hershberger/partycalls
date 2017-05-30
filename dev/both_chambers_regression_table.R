library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
new_whoheeds13[is.na(vote_share), vote_share := 100]
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]

setnames(new_whoheeds13, c("bestgrosswart", "power"),
  c("best_committee", "power_committee"))

hou_extremism <- pirate100 ~ ideological_extremism +  pfrate100 + vote_share +
  pres_vote_share + leader +  chair + power_committee + best_committee +
  female + afam + latino + south + seniority + freshman

sen_extremism <- pirate100 ~ ideological_extremism +  pfrate100 + vote_share +
  pres_vote_share + leader +  chair + power_committee + best_committee +
  female + afam + latino + south + seniority + freshman + up_for_reelection

texreg::screenreg(list(lm(hou_extremism, new_whoheeds13),
  lm(hou_extremism, senate_data),
  lm(sen_extremism, senate_data)),
  reorder.coef = c(2:16, 1))

texreg::texreg(list(lm(hou_extremism, new_whoheeds13),
  lm(hou_extremism, senate_data),
  lm(sen_extremism, senate_data)),
  reorder.coef = c(2:16, 1))

texreg::texreg(lm(sen_extremism2, senate_data))
