library(partycalls)
load("test_data/new_whoheeds13_lm.RData")

new_whoheeds13 <- new_whoheeds13[drop == 0,]

house_dem <- new_whoheeds13[dem == 1, ]
house_rep <- new_whoheeds13[dem == 0, ]
house_maj <- new_whoheeds13[majority == 1, ]
house_min <- new_whoheeds13[majority == 0, ]

stargazer::stargazer(new_whoheeds13, summary = TRUE)
stargazer::stargazer(house_dem, summary = TRUE)
stargazer::stargazer(house_rep, summary = TRUE)
stargazer::stargazer(house_maj, summary = TRUE)
stargazer::stargazer(house_min, summary = TRUE)
