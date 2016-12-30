library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

# load data for analysis
load("data/house_party_calls_replication_reassign_flip_flop_votes.RData")
whoheeds13 <- readstata13::read.dta13(
  "inst/extdata/who-heeds-replication-archive.dta")
setDT(new_whoheeds13)
setDT(whoheeds13)
new_whoheeds13 <- merge(new_whoheeds13, whoheeds13, by = c("congress", "icpsr"))
setDT(new_whoheeds13)

# regression formulas and functions
f.meddist <- new_pirate100 ~ new_distance_from_floor_median +
  new_pfrate100 + dpres_pct + south + votepct + female + afam + latino + seniority +
  freshman + retiree + bestgrosswart + leader + power + speaker + chair
f.extremism <- new_pirate100 ~ new_ideological_extremism +
  new_pfrate100 + dpres_pct + south.x + votepct.x + female.x + afam.x + latino.x +
  seniority.x + freshman.x + retiree + bestgrosswart + leader.x +
  power.x + chair.x

new_whoheeds13_dem97 <- new_whoheeds13[dem.x]

# make replication of who heeds 2013 table 3
texreg::screenreg(list(
  lm(f.extremism, new_whoheeds13[dem.x == 1 & congress == 97]),
  lm(f.extremism, new_whoheeds13[dem.x == 1 & congress == 102]),
  lm(f.extremism, new_whoheeds13[dem.x == 1 & congress == 107]),
  lm(f.extremism, new_whoheeds13[dem.x == 0 & congress == 97]),
  lm(f.extremism, new_whoheeds13[dem.x == 0 & congress == 102]),
  lm(f.extremism, new_whoheeds13[dem.x == 0 & congress == 107])
))

texreg::texreg(list(
  lm(f.extremism, new_whoheeds13[dem.x == 1 & congress == 97]),
  lm(f.extremism, new_whoheeds13[dem.x == 1 & congress == 102]),
  lm(f.extremism, new_whoheeds13[dem.x == 1 & congress == 107]),
  lm(f.extremism, new_whoheeds13[dem.x == 0 & congress == 97]),
  lm(f.extremism, new_whoheeds13[dem.x == 0 & congress == 102]),
  lm(f.extremism, new_whoheeds13[dem.x == 0 & congress == 107])
))
