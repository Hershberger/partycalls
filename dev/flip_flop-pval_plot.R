par(mfrow = c(2, 2))

hist(house_party_calls$hou103$record_of_pvals[[9]], breaks = seq(from = 0,
  to = 1, by = 0.0005), xlim = c(0.005, 0.015), ylim = c(0, 10), main = "")
abline(v = 0.01, col = "red")

hist(house_party_calls$hou103$record_of_pvals[[10]], breaks = seq(from = 0,
  to = 1, by = 0.0005), xlim = c(0.005, 0.015), ylim = c(0, 10), main = "")
abline(v = 0.01, col = "red")

hist(house_party_calls$hou103$record_of_pvals[[11]], breaks = seq(from = 0,
  to = 1, by = 0.0005), xlim = c(0.005, 0.015), ylim = c(0, 10), main = "")
abline(v = 0.01, col = "red")

hist(house_party_calls$hou103$record_of_pvals[[12]], breaks = seq(from = 0,
  to = 1, by = 0.0005), xlim = c(0.005, 0.015), ylim = c(0, 10), main = "")
abline(v = 0.01, col = "red")

par(mfrow = c(1, 1))
