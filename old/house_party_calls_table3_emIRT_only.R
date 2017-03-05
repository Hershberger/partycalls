# produce replication of Table 3 relationships MV13

library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/new_whoheeds13_emIRT_only.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]

f_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_votepct + south + votepct + female + afam + latino +
  seniority + freshman + bestgrosswart + leader +
  power + chair

texreg::screenreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0]),
  lm(f_extremism, new_whoheeds13[majority == 1]),
  lm(f_extremism, new_whoheeds13[majority == 0])
))

texreg::texreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0]),
  lm(f_extremism, new_whoheeds13[majority == 1]),
  lm(f_extremism, new_whoheeds13[majority == 0])
))

# make replication of who heeds 2013 table 3
texreg::screenreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 107])
))

texreg::texreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 107])
))
