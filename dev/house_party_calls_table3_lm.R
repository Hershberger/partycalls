# produce replication of Table 3 relationships MV13

library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
new_whoheeds13[, vote_share := vote_share - mean(vote_share, na.rm = TRUE)]
new_whoheeds13[is.na(vote_share) == TRUE, vote_share := 0]

f_extremism <- pirate100 ~ ideological_extremism +   pfrate100 +
  vote_share + pres_vote_share + leader + chair + power + bestgrosswart +
  female + afam + latino + south +
  south + seniority + freshman


texreg::screenreg(list(lm(f_extremism, new_whoheeds13),
  lm(f_extremism, new_whoheeds13[dem == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0]),
  lm(f_extremism, new_whoheeds13[majority == 1]),
  lm(f_extremism, new_whoheeds13[majority == 0])
), reorder.coef = c(2:15, 1), digits = 3)

texreg::texreg(list(lm(f_extremism, new_whoheeds13),
  lm(f_extremism, new_whoheeds13[dem == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0]),
  lm(f_extremism, new_whoheeds13[majority == 1]),
  lm(f_extremism, new_whoheeds13[majority == 0])
), reorder.coef = c(2:15, 1), digits = 3)

# make replication of who heeds 2013 table 3
texreg::screenreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 107])
), reorder.coef = c(2:15, 1))

texreg::texreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 107])
), reorder.coef = c(2:15, 1))
