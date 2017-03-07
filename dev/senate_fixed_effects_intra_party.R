library(partycalls)
library(lfe)

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]

mod1 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
  pres_vote_share + freshman + retiree  + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Democrat" & south == 1])

mod2 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
  pres_vote_share + freshman + retiree  + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Democrat" & south == 0])

mod3 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
  pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Republican" & gingrich_senator == 1])

mod4 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
  pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Republican" & gingrich_senator == 0])

texreg::screenreg(list(mod1, mod2, mod3, mod4))

texreg::texreg(list(mod1, mod2, mod3, mod4))
