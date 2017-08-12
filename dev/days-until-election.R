library(partycalls)
library(lfe)
library(mvtnorm)
library(ggplot2)

#' Calculate (cluster-)robust variance-covariance matrix
#'
#' Calculate (cluster-)robust variance-covariance matrix
#' @param model a fitted glm model
#' @param cluster1 vector or NULL
#' @param cluster2 vector or NULL
#' @return variance-covariance matrix
#' @export
#' @importFrom sandwich vcovHC estfun
robust_vcov <- function(model, cluster1 = NULL, cluster2 = NULL)
{
  bread <- vcov(model)
  u <- sandwich::estfun(model)
  k <- model$rank
  n <- length(model$residuals)
  if (is.null(cluster1)) {
    sandwich::vcovHC(model, type = "HC0")
  } else if (is.null(cluster2)) {
    n_g <- length(unique(cluster1))
    dfc <- n_g / (n_g - 1)
    u_clust <- apply(u, 2, tapply, cluster1, sum)
    meat <- dfc * crossprod(u_clust)
    bread %*% meat %*% bread
  } else {
    cluster12 <- paste(cluster1, cluster2)
    n_g1 <- length(unique(cluster1))
    n_g2 <- length(unique(cluster2))
    n_g12 <- length(unique(cluster12))
    dfc1 <- n_g1 / (n_g1 - 1)
    dfc2 <- n_g2 / (n_g2 - 1)
    dfc12 <- n_g12 / (n_g12 - 1)
    u_clust1 <- apply(u, 2, tapply, cluster1, sum)
    u_clust2 <- apply(u, 2, tapply, cluster2, sum)
    u_clust12 <- apply(u, 2, tapply, cluster12, sum)
    vc1 <- dfc1 * bread %*% crossprod(u_clust1) %*% bread
    vc2 <- dfc2 * bread %*% crossprod(u_clust2) %*% bread
    vc12 <- dfc12 * bread %*% crossprod(u_clust12) %*% bread
    #(n - 1) / (n - k) * (vc1 + vc2 - vc12)
    vc1 + vc2 - vc12
  }
}
calc_rse <- function(z) {
  lhs <- z$lhs
  if (is.null(lhs)) {
    lhs <- colnames(z$residuals)[1]
  }
  if (is.null(z$weights))  {
    w <- 1
  } else {
    w <- z$weights
  }
  residuals <- as.vector(w * z$residuals[, lhs])
  rss <- sum(residuals ^ 2)
  rdf <-  z$N - z$p
  resvar <- rss / rdf
  sigma <- sqrt(resvar)
  sigma
}

election_dates <- as.Date(c(
  "1974-11-05",
  "1976-11-02",
  "1978-11-07",
  "1980-11-04",
  "1982-11-02",
  "1984-11-06",
  "1986-11-04",
  "1988-11-08",
  "1990-11-06",
  "1992-11-04",
  "1994-11-08",
  "1996-11-05",
  "1998-11-03",
  "2000-11-07",
  "2002-11-05",
  "2004-11-02",
  "2006-11-07",
  "2008-11-04",
  "2010-11-02",
  "2012-11-06",
  "2014-11-04",
  "2016-11-08"
))

house_legis_votes_data <- rbindlist(lapply(1:20, function(i) {
  roll_calls_object_list <- house_party_calls
  congress <- i + 92
  chamber <- "house"
  # set up data to merge
  if (chamber == "house"){
    rc <- roll_calls_object_list[[paste0("hou", congress)]]
  } else if (chamber == "senate") {
    rc <- roll_calls_object_list[[paste0("sen", congress)]]
  }
  ld <- rc$legis.data
  ld$mc <- rownames(ld)
  setDT(ld)
  votes <- rc$votes
  votes <- melt(votes)
  data.table::setDT(votes)
  setnames(votes, c("mc", "vote_id", "vote"))
  votes <- merge(votes, ld, by = "mc")
  # make responsiveness data
  party_call_coding <- rc$party_call_coding$coding
  gray_votes <- votes$vote_id[which(party_call_coding == "gray")]
  party_calls <- votes$vote_id[which(party_call_coding == "party call")]
  noncalls <- votes$vote_id[which(party_call_coding == "noncall")]
  votes[, gray := as.numeric(vote_id %in% gray_votes)]
  votes[, party_call := as.numeric(vote_id %in% party_calls)]
  votes[, noncall := as.numeric(vote_id %in% noncalls)]
  votes[, party_yea_rate := sum(vote == 1) / sum(vote %in% c(1, -1)),
    .(vote_id, party)]
  votes[, party_pos :=
      as.numeric(party_yea_rate > .5) - as.numeric(party_yea_rate < .5)]
  votes[vote %in% c(1, -1), vote_with_party := as.numeric(vote == party_pos)]
  votes <- merge(votes,
    dcast(votes[party %like% "^D$|^R$", mean(party_pos), .(party, vote_id)],
      vote_id ~ party, value.var = "V1")[,
        .(party_vote = as.numeric(D %in% c(1, -1) & R %in% c(1, -1) & D != R)),
        vote_id], by = "vote_id")

  responsiveness_data <- votes[vote %in% c(1, -1) & party_pos != 0,
    .(
      responsiveness_party_calls =
        mean(vote[party_call == 1] == party_pos[party_call == 1]),
      responsiveness_noncalls =
        mean(vote[noncall == 1] == party_pos[noncall == 1]),
      n_party_calls = sum(party_call == 1),
      n_noncalls = sum(noncall == 1)
    ), mc]
  votes <- merge(votes, responsiveness_data, by = "mc")

  vd <- copy(rc$vote.data)
  vd[, vote_question := NULL]
  vd[, vote_desc := NULL]
  vd[, dtl_desc := NULL]
  setorder(vd, rollnumber)
  vd[, vote_id := colnames(rc$votes)]
  vd[, next_election_date := election_dates[i]]
  vd[, date := as.Date(date)]
  vd[, days_until_next_election :=
      as.numeric(next_election_date) - as.numeric(date)]
  vd[, after_election := 0]
  vd[days_until_next_election < 0, after_election := 1]
  vd[days_until_next_election < 0,
    next_election_date := election_dates[i + 1]]
  vd[days_until_next_election < 0, days_until_next_election :=
      as.numeric(next_election_date) - as.numeric(date)]
  # out <- merge(out, hdvd, by = "rollnumber")
  votes <- merge(votes, vd, by = "vote_id")
  votes
}))

house_legis_votes_data <- merge(house_legis_votes_data,
  house_data, by = c("congress", "icpsrLegis"))
house_legis_votes_data[,
  days_until_next_election_scaled := scale(days_until_next_election)]
house_legis_votes_data[, congress_vote_id := paste0(congress, "-", vote_id)]
house_legis_votes_data[, legis_congress_id := paste0(congress, "-", icpsrLegis)]

date_calls <- house_legis_votes_data[,
  .N, .(congress, vote_id, days_until_next_election,
    party_call = factor(party_call),
    party_vote = factor(party_vote))]
date_calls[,
  days_until_next_election := days_until_next_election + 5 * runif(.N) - 5]

# check for expected correlation
house_legis_votes_data[, .N, .(party_call, party_vote)]
house_legis_votes_data[, cor(party_call, party_vote)]

house_legis_votes_data[, non_party_vote := 1 - party_vote]

# party vote model
party_vote_model <- felm(vote_with_party ~
    party_vote * days_until_next_election_scaled +
    party_vote * I(days_until_next_election_scaled ^ 2) +
    party_vote * I(days_until_next_election_scaled ^ 3) |
    legis_congress_id | 0 | congress,
  house_legis_votes_data[after_election == 0 & gray != 1])
b <- coef(party_vote_model)
v <- vcov(party_vote_model)
rse <- calc_rse(party_vote_model)
party_vote_X <- CJ(party_vote = c(0, 1),
  days_until_next_election_scaled = seq(-1.7, 1.8, .1))
x <- copy(party_vote_X)
x[, `:=`(
  `I(days_until_next_election_scaled^2)` =
    days_until_next_election_scaled ^ 2,
  `I(days_until_next_election_scaled^3)` =
    days_until_next_election_scaled ^ 3,
  `party_vote:days_until_next_election_scaled` =
    party_vote * days_until_next_election_scaled,
  `party_vote:I(days_until_next_election_scaled^2)` =
    party_vote * days_until_next_election_scaled ^ 2,
  `party_vote:I(days_until_next_election_scaled^3)` =
    party_vote * days_until_next_election_scaled ^ 3)
  ]
party_vote_X[, y := as.matrix(x) %*% b]
class(party_vote_model) <- "lm"
sims <- as.matrix(x) %*% t(mvrnorm(1000, b, v))
party_vote_X[, ymin := apply(sims, 1, quantile, .0825)]
party_vote_X[, ymax := apply(sims, 1, quantile, .9175)]
party_vote_X[, party_vote := factor(party_vote)]
m <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:center")]
s <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:scale")]
party_vote_X[, days_until_next_election := days_until_next_election_scaled * s + m]

ggplot(party_vote_X, aes(days_until_next_election,
  group = party_vote, color = party_vote, fill = party_vote)) +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .4, color = NA) +
  geom_line(aes(y = y, linetype = party_vote)) +
  geom_rug(data = date_calls[party_vote == 1],
    alpha = .01, sides = "b") +
  geom_rug(data = date_calls[party_vote == 0],
    alpha = .01, sides = "t") +
  ylab("Estimated Difference from Non-party Vote at Midpoint of Term") +
  xlab("Days Until Next Election") +
  theme_minimal() +
  scale_x_reverse() #+
  # theme(legend.position = "none") +
  # annotate("text", 550, .05, label = "Non-party call") +
  # annotate("text", 475, -.02, label = "Party call")






# party call model
party_call_model <- felm(vote_with_party ~
    party_call * days_until_next_election_scaled +
    party_call * I(days_until_next_election_scaled ^ 2) +
    party_call * I(days_until_next_election_scaled ^ 3) |
    legis_congress_id | 0 | congress,
  house_legis_votes_data[after_election == 0 & gray != 1])
b <- coef(party_call_model)
v <- vcov(party_call_model)
party_call_X <- CJ(party_call = c(0, 1),
  days_until_next_election_scaled = seq(-1.7, 1.8, .1))
x <- copy(party_call_X)
x[, `:=`(
  `I(days_until_next_election_scaled^2)` =
    days_until_next_election_scaled ^ 2,
  `I(days_until_next_election_scaled^3)` =
    days_until_next_election_scaled ^ 3,
  `party_call:days_until_next_election_scaled` =
    party_call * days_until_next_election_scaled,
  `party_call:I(days_until_next_election_scaled^2)` =
    party_call * days_until_next_election_scaled ^ 2,
  `party_call:I(days_until_next_election_scaled^3)` =
    party_call * days_until_next_election_scaled ^ 3)
  ]
party_call_X[, y := as.matrix(x) %*% b]
sims <- as.matrix(x) %*% t(mvrnorm(1000, b, v))
party_call_X[, ymin := apply(sims, 1, quantile, .0825)]
party_call_X[, ymax := apply(sims, 1, quantile, .9175)]
party_call_X[, party_call := factor(party_call)]
m <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:center")]
s <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:scale")]
party_call_X[,
  days_until_next_election := days_until_next_election_scaled * s + m]

ggplot(party_call_X, aes(days_until_next_election,
  group = party_call, color = party_call, fill = party_call)) +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .4, color = NA) +
  geom_line(aes(y = y, linetype = party_call)) +
  geom_rug(data = date_calls[party_call == 1],
    alpha = .01, sides = "b") +
  geom_rug(data = date_calls[party_call == 0],
    alpha = .01, sides = "t") +
  ylab("Estimated Difference from Non-Party Call at Midpoint of Term") +
  xlab("Days Until Next Election") +
  theme_minimal() +
  scale_x_reverse() #+
# theme(legend.position = "none") +
# annotate("text", 550, .05, label = "Non-party call") +
# annotate("text", 475, -.02, label = "Party call")




combined_model <- felm(vote_with_party ~
    party_call * party_vote * days_until_next_election_scaled +
    party_call * party_vote * I(days_until_next_election_scaled ^ 2) +
    party_call * party_vote * I(days_until_next_election_scaled ^ 3) |
    legis_congress_id | 0 | congress,
  house_legis_votes_data[after_election == 0 & gray != 1])

b <- coef(combined_model)
v <- vcov(combined_model)
combined_X <- CJ(
  party_call = c(0, 1),
  party_vote = c(0, 1),
  days_until_next_election_scaled = seq(-1.7, 1.8, .1))
x <- combined_X[, .(
  party_call,
  party_vote,
  days_until_next_election_scaled,
  days_until_next_election_scaled ^ 2,
  days_until_next_election_scaled ^ 3,
  party_call * party_vote,
  party_call * days_until_next_election_scaled,
  party_vote * days_until_next_election_scaled,
  party_call * days_until_next_election_scaled ^ 2,
  party_vote * days_until_next_election_scaled ^ 2,
  party_call * days_until_next_election_scaled ^ 3,
  party_vote * days_until_next_election_scaled ^ 3,
  party_vote * party_call * days_until_next_election_scaled,
  party_vote * party_call * days_until_next_election_scaled ^ 2,
  party_vote * party_call * days_until_next_election_scaled ^ 3)
  ]
combined_X[, y := as.matrix(x) %*% b]
sims <- as.matrix(x) %*% t(mvrnorm(1000, b, v))
combined_X[, ymin := apply(sims, 1, quantile, .0825)]
combined_X[, ymax := apply(sims, 1, quantile, .9175)]
combined_X[, party_vote := factor(party_vote)]
combined_X[, party_call := factor(party_call)]
m <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:center")]
s <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:scale")]
combined_X[,
  days_until_next_election := days_until_next_election_scaled * s + m]


ggplot(combined_X, aes(days_until_next_election,
  group = paste(party_call, party_vote),
  color = paste(party_call, party_vote),
  fill = paste(party_call, party_vote))) +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .4, color = NA) +
  geom_line(aes(y = y, linetype = paste(party_call, party_vote))) +
  # geom_rug(data = date_calls[party_call == 1],
  #   alpha = .01, sides = "b") +
  # geom_rug(data = date_calls[party_call == 0],
  #   alpha = .01, sides = "t") +
  ylab("Effect of Election Proximity") +
  xlab("Days Until Next Election") +
  theme_minimal() +
  scale_x_reverse() #+
# theme(legend.position = "none") +
# annotate("text", 550, .05, label = "Non-party call") +
# annotate("text", 475, -.02, label = "Party call")



library(speedglm)
combined_glm <- glm(vote_with_party ~
    party_call * party_vote * days_until_next_election_scaled +
    party_call * party_vote * I(days_until_next_election_scaled ^ 2) +
    party_call * party_vote * I(days_until_next_election_scaled ^ 3),
  family = binomial(),
  data = house_legis_votes_data[after_election == 0 & gray != 1])
b <- coef(combined_glm)
v <- robust_vcov(combined_glm,
  house_legis_votes_data[
    after_election == 0 & gray != 1,
    congress][-attr(combined_glm$model, "na.action")])
combined_X <- CJ(
  party_call = c(0, 1),
  party_vote = c(0, 1),
  days_until_next_election_scaled = seq(-1.7, 1.8, .1))
x <- combined_X[, .(
  1,
  party_call,
  party_vote,
  days_until_next_election_scaled,
  days_until_next_election_scaled ^ 2,
  days_until_next_election_scaled ^ 3,
  party_call * party_vote,
  party_call * days_until_next_election_scaled,
  party_vote * days_until_next_election_scaled,
  party_call * days_until_next_election_scaled ^ 2,
  party_vote * days_until_next_election_scaled ^ 2,
  party_call * days_until_next_election_scaled ^ 3,
  party_vote * days_until_next_election_scaled ^ 3,
  party_vote * party_call * days_until_next_election_scaled,
  party_vote * party_call * days_until_next_election_scaled ^ 2,
  party_vote * party_call * days_until_next_election_scaled ^ 3)
  ]
combined_X[, y := plogis(as.matrix(x) %*% b)]
sims <- as.matrix(x) %*% t(mvrnorm(1000, b, v))
sims <- matrix(plogis(sims), nrow(sims), ncol(sims))
combined_X[, ymin := apply(sims, 1, quantile, .0825)]
combined_X[, ymax := apply(sims, 1, quantile, .9175)]
combined_X[, party_vote := factor(party_vote)]
combined_X[, party_call := factor(party_call)]
m <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:center")]
s <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:scale")]
combined_X[,
  days_until_next_election := days_until_next_election_scaled * s + m]

ggplot(combined_X, aes(days_until_next_election,
  group = paste(party_call, party_vote),
  color = paste(party_call, party_vote),
  fill = paste(party_call, party_vote))) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .4, color = NA) +
  geom_line(aes(y = y, linetype = paste(party_call, party_vote))) +
  # geom_rug(data = date_calls[party_call == 1],
  #   alpha = .01, sides = "b") +
  # geom_rug(data = date_calls[party_call == 0],
  #   alpha = .01, sides = "t") +
  ylab("Effect of Election Proximity") +
  xlab("Days Until Next Election") +
  theme_minimal() +
  scale_x_reverse()
# theme(legend.position = "none") +
# annotate("text", 550, .05, label = "Non-party call") +
# annotate("text", 475, -.02, label = "Party call")





# party vote model
party_vote_glm <- glm(vote_with_party ~
    party_vote * days_until_next_election_scaled +
    party_vote * I(days_until_next_election_scaled ^ 2) +
    party_vote * I(days_until_next_election_scaled ^ 3) +
    dem + party_free_ideal_point +
    seniority +
    vote_share +
    pres_vote_share,
  family = binomial(),
  house_legis_votes_data[after_election == 0 & gray != 1])
b <- coef(party_vote_glm)
v <- robust_vcov(party_vote_glm,
  house_legis_votes_data[
    after_election == 0 & gray != 1,
    congress][-attr(party_vote_glm$model, "na.action")])
party_vote_X <- CJ(party_vote = c(0, 1),
  days_until_next_election_scaled = seq(-1.7, 1.8, .1))
x <- copy(party_vote_X)
x[, `:=`(
  `I(days_until_next_election_scaled^2)` =
    days_until_next_election_scaled ^ 2,
  `I(days_until_next_election_scaled^3)` =
    days_until_next_election_scaled ^ 3,
  `party_vote:days_until_next_election_scaled` =
    party_vote * days_until_next_election_scaled,
  `party_vote:I(days_until_next_election_scaled^2)` =
    party_vote * days_until_next_election_scaled ^ 2,
  `party_vote:I(days_until_next_election_scaled^3)` =
    party_vote * days_until_next_election_scaled ^ 3)
  ]
party_vote_X[, y := as.matrix(x) %*% b]
sims <- as.matrix(x) %*% t(mvrnorm(1000, b, v))
party_vote_X[, ymin := apply(sims, 1, quantile, .025)]
party_vote_X[, ymax := apply(sims, 1, quantile, .975)]
party_vote_X[, party_vote := factor(party_vote)]
m <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:center")]
s <- house_legis_votes_data[,
  attr(days_until_next_election_scaled,"scaled:scale")]
party_vote_X[, days_until_next_election := days_until_next_election_scaled * s + m]

ggplot(party_vote_X, aes(days_until_next_election,
  group = party_vote, color = party_vote, fill = party_vote)) +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .4, color = NA) +
  geom_line(aes(y = y, linetype = party_vote)) +
  geom_rug(data = date_calls[party_vote == 1],
    alpha = .01, sides = "b") +
  geom_rug(data = date_calls[party_vote == 0],
    alpha = .01, sides = "t") +
  ylab("Effect of Election Proximity") +
  xlab("Days Until Next Election") +
  theme_minimal() +
  scale_x_reverse() #+
# theme(legend.position = "none") +
# annotate("text", 550, .05, label = "Non-party call") +
# annotate("text", 475, -.02, label = "Party call")




## linear regression
x1 <- sin(1:10)
x2 <- cos(1:10)
y <- rnorm(10)
fm <- lm(y ~ x1 + x2)

## estimating function: (y - x'beta) * x
sandwich::estfun(fm)
residuals(fm) * cbind(1, x1, x2)
