library(senatePartyCalls)
summary(lm(responsiveness_party_calls ~
    ideological_extremism * gingrich_senator +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree + seniority +
    leader + chair + best_committee + power_committee +
    south13 + afam + female + latino,
  data = senator_year_data))
senator_year_data[gingrich_senator == 1,
  coef(lm(responsiveness_party_calls ~ responsiveness_noncalls))$]
senator_year_data[gingrich_senator == 0, mean(responsiveness_party_calls)]
