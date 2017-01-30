library(partycalls)
load("old_senatePartyCalls/old_data/senator_year_data.rda")

senate_data <- senator_year_data[drop == 0,]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]
# senate_maj <- senate_data[maj == 1, ]
# senate_min <- senate_data[maj == 0, ]

stargazer::stargazer(senate_dem, summary = TRUE)
stargazer::stargazer(senate_rep, summary = TRUE)
# stargazer::stargazer(senate_maj, summary = TRUE)
# stargazer::stargazer(senate_min, summary = TRUE)

