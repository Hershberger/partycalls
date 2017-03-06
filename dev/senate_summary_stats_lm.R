library(partycalls)
load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0,]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]
senate_maj <- senate_data[maj == 1, ]
senate_min <- senate_data[maj == 0, ]

stargazer::stargazer(senate_data, summary = TRUE)
stargazer::stargazer(senate_dem, summary = TRUE)
stargazer::stargazer(senate_rep, summary = TRUE)
stargazer::stargazer(senate_maj, summary = TRUE)
stargazer::stargazer(senate_min, summary = TRUE)

