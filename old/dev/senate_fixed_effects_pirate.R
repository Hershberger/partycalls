library(partycalls)
library(lfe)

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]

mod1 <- felm(pirate100 ~ ideological_extremism + vote_share +
  pres_vote_share + freshman + retiree  + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Democrat"])

mod2 <- felm(pirate100 ~ ideological_extremism + vote_share +
  pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Republican"])

mod3 <- felm(pirate100 ~ ideological_extremism + vote_share +
  pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
  power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[maj == 1])

mod4 <- felm(pirate100 ~ ideological_extremism + vote_share +
  pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
  power_committee + leader | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[maj == 0])

texreg::screenreg(list(mod1, mod2, mod3, mod4))

texreg::texreg(list(mod1, mod2, mod3, mod4))
