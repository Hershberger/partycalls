library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

# load data for analysis
load("data/")

# regression formulas and functions
f.meddist <- new_pirate100 ~ new_distance_from_floor_median +
  new_pfrate100 + dpres + south + votepct + female + afam + latino + seniority +
  freshman + retiree + bestgrosswart + leader + power + speaker + chair
f.extremism <- new_pirate100 ~ new_ideological_extremism +
  new_pfrate100 + dpres + south + votepct + female + afam + latino + seniority +
  freshman + retiree + bestgrosswart + leader + power + speaker + chair

# make replication of who heeds 2013 table 3
texreg::screenreg(list(
  lm(f.extremism, [dem == 0 & congress == 97]),
  lm(f.extremism, [dem == 0 & congress == 102]),
  lm(f.extremism, [dem == 0 & congress == 107]),
  lm(f.extremism, [dem == 0 & congress == 97]),
  lm(f.extremism, [dem == 0 & congress == 102]),
  lm(f.extremism, [dem == 0 & congress == 107])
))
