library(senatePartyCalls)
library(ggplot2)
library(lfe)

m1 <- felm(responsiveness_party_calls ~ dist_from_floor_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee |
    icpsrLegis + congress | 0 | icpsrLegis,
  senator_year_data[caucus == "Democrat" & drop == 0])

m2 <- felm(responsiveness_party_calls ~ dist_from_floor_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis,
  senator_year_data[caucus == "Republican" & drop == 0])

m3a <- felm(responsiveness_party_calls ~ ideological_extremism +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis,
  senator_year_data[caucus == "Democrat" & drop == 0])

m4 <- felm(responsiveness_party_calls ~ ideological_extremism +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis,
  senator_year_data[caucus == "Republican" & drop == 0])

m5 <- felm(responsiveness_party_calls ~ dist_from_party_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis,
  senator_year_data[caucus == "Democrat" & drop == 0])

m6 <- felm(responsiveness_party_calls ~ dist_from_party_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis,
  senator_year_data[caucus == "Republican" & drop == 0])

texreg::screenreg(list(m1, m2, m3a, m4, m5, m6),
  reorder.coef = c(1, 13, 14, 2:4, 12, 5:11),
  digits = 3)

m7 <- felm(responsiveness_party_calls ~ dist_from_floor_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0 & gingrich_senator == 1])

m8 <- felm(responsiveness_party_calls ~  ideological_extremism +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0 & gingrich_senator == 1])

m9 <- felm(responsiveness_party_calls ~  dist_from_party_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0 & gingrich_senator == 1])

m10 <- felm(responsiveness_party_calls ~ dist_from_floor_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0 & gingrich_senator == 0])

m11 <- felm(responsiveness_party_calls ~ ideological_extremism +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0 & gingrich_senator == 0])

m12 <- felm(responsiveness_party_calls ~  dist_from_party_median +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0 & gingrich_senator == 0])


texreg::screenreg(list(m7, m8, m9, m10, m11, m12),
  reorder.coef = c(1, 13, 14, 2:4, 12, 5:11),
  digits = 3)

m13 <- felm(responsiveness_party_calls ~
    dist_from_floor_median * gingrich_senator - gingrich_senator +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0])

m14 <- felm(responsiveness_party_calls ~
    ideological_extremism * gingrich_senator - gingrich_senator +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0])

m15 <- felm(responsiveness_party_calls ~
    dist_from_party_median * gingrich_senator - gingrich_senator +
    responsiveness_noncalls + vote_share * pres_vote_share +
    up_for_reelection + freshman + retiree +
    leader + chair + best_committee + power_committee  |
    icpsrLegis + congress  | 0 | icpsrLegis ,
  senator_year_data[caucus == "Republican" & drop == 0])


texreg::screenreg(list(m13, m14, m15),
  reorder.coef = c(1, 12, 14:17, 2:4, 13, 5:11),
  digits = 3)
