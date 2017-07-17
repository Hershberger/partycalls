library(partycalls)

load("test_data/senate_data_p_05.RData")

senate_data <- senate_data[drop == 0, ]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]

dem_extreme <- senate_dem[ideological_extremism > 1, ]
rep_extreme <- senate_rep[ideological_extremism > 1, ]

dem_extreme_noncomplier <- dem_extreme[pirate100 < 75, ]
rep_extreme_noncomplier <- rep_extreme[pirate100 < 75, ]

summary(dem_extreme_noncomplier$maj)
summary(rep_extreme_noncomplier$maj)

summary(dem_extreme_noncomplier$freshman)
summary(rep_extreme_noncomplier$freshman)

summary(dem_extreme_noncomplier$south)
summary(rep_extreme_noncomplier$gingrich_senator)

summary(dem_extreme_noncomplier$up_for_reelection)
summary(rep_extreme_noncomplier$up_for_reelection)

summary(dem_extreme_noncomplier$votes)
summary(rep_extreme_noncomplier$votes)

unique(dem_extreme_noncomplier$mc)
unique(rep_extreme_noncomplier$mc)

# only 4 congresses for dems in this group, most in Congress 97
unique(dem_extreme_noncomplier$congress)
table(dem_extreme_noncomplier$maj, dem_extreme_noncomplier$congress)
# more for reps, and more spread
unique(rep_extreme_noncomplier$congress)
table(rep_extreme_noncomplier$maj, rep_extreme_noncomplier$congress)

# generate tables with relevant traits reported
dem_noncomplier_table <- dem_extreme_noncomplier[, .(congress, mc, votes,
  pres_vote_share, ideological_extremism, pfrate100, pirate100)]
xtable(dem_noncomplier_table)

rep_noncomplier_table <- rep_extreme_noncomplier[, .(congress, mc, votes,
  pres_vote_share, ideological_extremism, pfrate100, pirate100)]
xtable(rep_noncomplier_table)

