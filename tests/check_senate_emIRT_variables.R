library(partycalls)

load("test_data/senator_year_data_emIRT_only.RData")

# test if vote share variables are the same and if not where errors are
plot((senator_year_data$vote_share * 100), senator_year_data$votepct)

# make sure missing values are shared
senator_year_data[is.na(vote_share) == TRUE & is.na(votepct) == FALSE,
  congress, icpsrLegis]
senator_year_data[is.na(vote_share) == FALSE & is.na(votepct) == TRUE,
  congress, icpsrLegis]

# plot each against presidential vote share
plot((senator_year_data$pres_vote_share * 100), (senator_year_data$vote_share * 100))

plot((senator_year_data$pres_vote_share * 100), senator_year_data$votepct)

# figure out who the people with < 50% of the vote are
votepct_to_check_1 <-
  senator_year_data[votepct <= 50, .(congress, icpsrLegis, stabb, last_name, votepct)]

# checking republican ideological extremism and party free ideal point
senator_year_data[caucus == "Republican" &
    ideological_extremism != party_free_ideal_point, .(congress, icpsrLegis,
      stabb, last_name, ideological_extremism, party_free_ideal_point)]
senator_year_data[caucus == "Republican", mean(ideological_extremism)]
senator_year_data[caucus == "Republican", mean(party_free_ideal_point)]

# and democrats
senator_year_data[caucus == "Democrat" &
    ideological_extremism != (party_free_ideal_point * -1) ,]

# making sure no one reappears after retiring
senator_year_data[retiree == 1, .(congress, icpsrLegis)]

senator_year_data[icpsrLegis == 8764 & congress > 95, ]
senator_year_data[icpsrLegis == 12100 & congress > 95, ]
senator_year_data[icpsrLegis == 11200 & congress > 98, ]
senator_year_data[icpsrLegis == 14930 & congress > 99, ]
senator_year_data[icpsrLegis == 7638 & congress > 100, ]
senator_year_data[icpsrLegis == 14105 & congress > 107, ]
senator_year_data[icpsrLegis == 15503 & congress > 108, ]
senator_year_data[icpsrLegis == 15501 & congress > 111, ]
