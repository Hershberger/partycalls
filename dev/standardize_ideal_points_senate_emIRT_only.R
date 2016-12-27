# load senator year data
load("data/senator_year_data_emIRT_only.RData")

# rescale ideal point mean to 0
senator_year_data$ideological_extremism <-
  senator_year_data$ideological_extremism - mean(senator_year_data$ideological_extremism)

# rescale ideal point standard deviation to 1
senator_year_data$ideological_extremism <-
  senator_year_data$ideological_extremism / sd(senator_year_data$ideological_extremism)

save(senator_year_data,
  file = "data/senator_year_data_emIRT_only.RData")
