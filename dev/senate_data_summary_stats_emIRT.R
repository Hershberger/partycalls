library(partycalls)

load("test_data/senate_data_emIRT_only.RData")
setDT(senate_data)

senate_data <- senate_data[, .(congress, class, pres_vote_share, votepct,
  south, leader, chair, best_committee, power_committee, up_for_reelection,
  freshman, seniority, retiree, afam, latino, female, maj, caucus,
  party_free_ideal_point, pirate100, pfrate100, ideological_extremism,
  icpsrLegis)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]
senate_maj <- senate_data[maj == 1, ]
senate_min <- senate_data[maj == 0, ]

# weicker listed as chair in majority party, congress 100
senate_min[chair == 1, icpsrLegis, congress]
senate_min[icpsrLegis == 12032, chair := 0]

stargazer::stargazer(senate_dem, summary = TRUE)
stargazer::stargazer(senate_rep, summary = TRUE)
stargazer::stargazer(senate_maj, summary = TRUE)
stargazer::stargazer(senate_min, summary = TRUE)

