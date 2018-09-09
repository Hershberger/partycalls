library(campaignfinance)
library(partycalls)
prev_house_data <- copy(partycalls::house_data)
next_house_data <- copy(partycalls::house_data)
prev_house_data <- prev_house_data[congress < 112]
next_house_data <- next_house_data[congress > 93]
prev_house_data[, congress := congress + 1]
data <- merge(next_house_data, prev_house_data,
  by = c("icpsrLegis", "congress"))
data[, voteshare := dv.x]
data[dem.x == 1, voteshare := 100-dv.x]
summary(lm(voteshare ~ responsiveness_to_party_calls.y, data))
