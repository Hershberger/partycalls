library(partycalls)
library(stargazer)

load("test_data/senate_data_emIRT_only.RData")

senate_data_dem <- senate_data[caucus == "Democrat",]
senate_data_rep <- senate_data[caucus == "Republican", ]

stargazer(senate_data_dem, summary = TRUE, summary.stat = c("mean", "sd"))
stargazer(senate_data_rep, summary = TRUE, summary.stat = c("mean", "sd"))
