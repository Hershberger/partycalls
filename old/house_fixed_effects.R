library(partycalls)
library(lfe)

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]

mod1 <- felm(pirate100 ~ ideological_extremism + pfrate100 + pres_votepct +
  votepct + freshman + bestgrosswart + leader + power + chair | icpsrLegis +
  congress | 0 | icpsrLegis, new_whoheeds13[dem == 1])

mod2 <- felm(pirate100 ~ ideological_extremism + pfrate100 + pres_votepct +
  votepct + freshman + bestgrosswart + leader + power + chair | icpsrLegis +
  congress | 0 | icpsrLegis, new_whoheeds13[dem == 0])

mod3 <- felm(pirate100 ~ ideological_extremism + pfrate100 + pres_votepct +
  votepct + freshman + bestgrosswart + leader + power + chair | icpsrLegis +
  congress | 0 | icpsrLegis, new_whoheeds13[majority == 1])

mod4 <- felm(pirate100 ~ ideological_extremism + pfrate100 + pres_votepct +
  votepct + freshman + bestgrosswart + leader + power + chair | icpsrLegis +
  congress | 0 | icpsrLegis, new_whoheeds13[majority == 0])

texreg::screenreg(list(mod1, mod2, mod3, mod4))

texreg::texreg(list(mod1, mod2, mod3, mod4))
