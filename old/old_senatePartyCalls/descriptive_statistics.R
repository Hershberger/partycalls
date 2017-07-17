library(senatePartyCalls)
library(stargazer)
library(data.table)
stargazer(senator_year_data[drop == 0], summary = TRUE)
