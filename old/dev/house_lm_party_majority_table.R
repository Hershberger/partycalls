library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
new_whoheeds13[is.na(vote_share) == TRUE, vote_share := 100]

f_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share + female + afam + latino +
  seniority + freshman + bestgrosswart + leader +
  power + chair

texreg::screenreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & majority == 1]),
  lm(f_extremism, new_whoheeds13[dem == 1 & majority == 0]),
  lm(f_extremism, new_whoheeds13[dem == 0 & majority == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0 & majority == 0])
), reorder.coef = c(2:3, 6, 4, 5, 7:15, 1))

texreg::texreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & majority == 1]),
  lm(f_extremism, new_whoheeds13[dem == 1 & majority == 0]),
  lm(f_extremism, new_whoheeds13[dem == 0 & majority == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0 & majority == 0])
), reorder.coef = c(2:3, 6, 4, 5, 7:15, 1))
