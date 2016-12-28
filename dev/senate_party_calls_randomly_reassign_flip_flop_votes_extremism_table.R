library(partycalls)
library(texreg)
library(data.table)
options(stringsAsFactors = FALSE)

# load data for analysis
load("data/senator_year_data_randomly_reassign_flip_flop_votes.RData")

f.extremism <- responsiveness_party_calls ~ ideological_extremism +
  responsiveness_noncalls + vote_share + pres_vote_share + south13 + female +
  afam + latino + up_for_reelection + seniority + freshman + retiree +
  best_committee + leader +  power_committee + chair

texreg::screenreg(list(lm(f.extremism, senator_year_data[caucus == "Democrat" & drop == 0]),
  lm(f.extremism, senator_year_data[caucus == "Republican" & drop == 0])),
  reorder.coef = c(2, 17, 3:5, 16, 6:15, 1), digits = 3)
texreg::screenreg(list(lm(f.extremism, senator_year_data[majority == 1 & drop == 0]),
  lm(f.extremism, senator_year_data[majority == 0 & drop == 0])),
  reorder.coef = c(2, 17, 3:5, 16, 6:15, 1), digits = 3)

texreg::texreg(list(lm(f.extremism, senator_year_data[caucus == "Democrat" & drop == 0]),
  lm(f.extremism, senator_year_data[caucus == "Republican" & drop == 0])),
  reorder.coef = c(2, 17, 3:5, 16, 6:15, 1), digits = 3)
texreg::texreg(list(lm(f.extremism, senator_year_data[caucus == "Democrat" & drop == 0]),
  lm(f.extremism, senator_year_data[caucus == "Republican" & drop == 0])),
  reorder.coef = c(2, 17, 3:5, 16, 6:15, 1), digits = 3)
