library(partycalls)
date_call <- rbindlist(lapply(1:20, function(i) {
  y <- rep(0, house_party_calls[[i]]$m)
  y[house_party_calls[[i]]$party_calls] <- 1
  x <- as.numeric(house_party_calls[[i]]$vote.data$date)
  x <- x - min(x)
  data.table(congress = i + 92,
    date = x,
    party_call = y)
}))

library(arm)
model <- glmer(party_call ~ (date | congress),
  family = binomial,
  data = date_call)

library(ggplot2)
ggplot(date_call, aes(date, party_call)) +
  geom_smooth(aes(group = congress), se = FALSE, alpha = .2, color = "gray") +
  geom_smooth(se = FALSE) +
  geom_rug(data = date_call[party_call == 1], alpha = .01, sides = "t") +
  geom_rug(data = date_call[party_call == 0], alpha = .01, sides = "b") +
  ylim(c(0, 1))
