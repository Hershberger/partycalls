library(data.table)
library(partycalls)
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
#' Calculate p values based on a given variance-covariane matrix
#'
#' Calculate p values based on a given variance-covariane matrix
#' @param model a fitted glm model
#' @param cluster1 vector or NULL
#' @param cluster2 vector or NULL
#' @return p values
#' @export
#' @importFrom lmtest coeftest
get_pval <- function(model, vcov)
{
  lmtest::coeftest(model, vcov)[, 4]
}

fox_news <- read.delim("inst/extdata/FoxNews_Master.tab", sep = "\t")
setDT(fox_news)

coding105 <- house_party_calls[[105 - 92]]$party_call_coding
coding105[, Votenum := as.numeric(substr(voteno, 6, 100))]
fox_news105 <- merge(fox_news[cong == 105], coding105, by = "Votenum", all.x = TRUE)

coding106 <- house_party_calls[[106 - 92]]$party_call_coding
coding106[, Votenum := as.numeric(substr(voteno, 6, 100))]
fox_news106 <- merge(fox_news[cong == 106], coding106, by = "Votenum", all.x = TRUE)

coding107 <- house_party_calls[[107 - 92]]$party_call_coding
coding107[, Votenum := as.numeric(substr(voteno, 6, 100))]
fox_news107 <- merge(fox_news[cong == 107], coding107, by = "Votenum", all.x = TRUE)

fox_news <- rbind(fox_news105, fox_news106, fox_news107)
fox_news[, party_call := as.numeric(coding == "party call")]
fox_news[coding == "gray", party_call := NA]
fox_news[, s_daystoelection := scale(daystoelection)]

model <- glm(PartyVote ~
    daystoelection * FoxNews +
    I(daystoelection ^ 2) * FoxNews +
    I(daystoelection ^ 3) * FoxNews +
    Retirement + seniorit + voteshare_lag  +
    qualchal_lag + qualchal + spendgap_lag + spendgap + distpart_lag +
    RegPass + Susp + OtherPass + Amend + ProPart,
  data = fox_news[PresencePartyUnity == 1 & Republican == 0],
  family = binomial)
coef(model)

model <- glm(PartyVote ~
    party_call +
    daystoelection * FoxNews +
    I(daystoelection ^ 2) * FoxNews +
    I(daystoelection ^ 3) * FoxNews +
    Retirement + seniorit + voteshare_lag  +
    qualchal_lag + qualchal + spendgap_lag + spendgap + distpart_lag +
    RegPass + Susp + OtherPass + Amend + ProPart,
  data = fox_news[PresencePartyUnity == 1 & Republican == 0] & coding != "gray",
  family = binomial)

summary(model)

model <- glm(PartyVote ~
    party_call * FoxNews +
    daystoelection * FoxNews +
    I(daystoelection ^ 2) * FoxNews +
    I(daystoelection ^ 3) * FoxNews +
    Retirement + seniorit + voteshare_lag  +
    qualchal_lag + qualchal + spendgap_lag + spendgap + distpart_lag +
    RegPass + Susp + OtherPass + Amend + ProPart,
  data = fox_news[PresencePartyUnity == 1 & Republican == 0 & coding != "gray"],
  family = binomial)

summary(model)


model <- glm(PartyVote ~
    party_call * s_daystoelection +
    party_call * I(s_daystoelection ^ 2) +
    party_call * I(s_daystoelection ^ 3) +
    Retirement + seniorit + voteshare_lag  +
    qualchal_lag + qualchal + spendgap_lag + spendgap + distpart_lag +
    RegPass + Susp + OtherPass + Amend + ProPart,
  data = fox_news[PresencePartyUnity == 1 & Republican == 0 & coding != "gray"],
  family = binomial)
V <- robust_vcov(model)
se <- diag(V) ^ .5
p <- get_pval(model, V)
texreg::screenreg(model, override.se = se, override.pvalues = p)
curve(1.26 + x * -.44 + x ^ 2 * -.05 + x^3 * .3, from = -2, to = 2)




model <- glm(PartyVote ~
    FoxNews * party_call * s_daystoelection +
    FoxNews * party_call * I(s_daystoelection ^ 2) +
    FoxNews * party_call * I(s_daystoelection ^ 3) +
    Retirement + seniorit + voteshare_lag  +
    qualchal_lag + qualchal + spendgap_lag + spendgap + distpart_lag +
    RegPass + Susp + OtherPass + Amend + ProPart,
  data = fox_news[PresencePartyUnity == 1 & Republican == 0 & coding != "gray"],
  family = binomial)
V <- robust_vcov(model)
se <- diag(V) ^ .5
p <- get_pval(model, V)
texreg::screenreg(model, override.se = se, override.pvalues = p)
# look at effect of party call by foxnews

newdata <- CJ(FoxNews = 0:1, party_call = 0:1, s_daystoelection = seq(-2, 2, .25),
  Retirement = 0,
  seniorit = fox_news[, mean(seniorit)],
  voteshare_lag = fox_news[, mean(voteshare_lag)],
  qualchal_lag = 1,
  qualchal = 1,
  spendgap_lag = fox_news[, mean(spendgap_lag)],
  spendgap = fox_news[, mean(spendgap)],
  distpart_lag = fox_news[, mean(distpart_lag)],
  RegPass = 1,
  Susp = 0,
  OtherPass= 0,
  Amend = 0,
  ProPart = 0)
newdata[, yhat := predict(model, newdata)]
newdata[FoxNews == 1 & party_call == 1, group := "Fox News-party call"]
newdata[FoxNews == 1 & party_call == 0, group := "Fox News-non call"]
newdata[FoxNews == 0 & party_call == 1, group := "no Fox News-party call"]
newdata[FoxNews == 0 & party_call == 0, group := "no Fox News-non call"]
ggplot(newdata, aes(s_daystoelection, plogis(yhat),
  group = group, color = factor(party_call), linetype = factor(FoxNews))) +
  geom_line() + ylim(.25, 1)



model <- glm(PartyVote ~
    FoxNews * party_call * s_daystoelection +
    FoxNews * party_call * I(s_daystoelection ^ 2) +
    FoxNews * party_call * I(s_daystoelection ^ 3) +
    Retirement + seniorit + voteshare_lag  +
    qualchal_lag + qualchal + spendgap_lag + spendgap + distpart_lag +
    RegPass + Susp + OtherPass + Amend + ProPart,
  data = fox_news[PresencePartyUnity == 1 & Republican == 1 & coding != "gray"],
  family = binomial)
V <- robust_vcov(model)
se <- diag(V) ^ .5
p <- get_pval(model, V)
texreg::screenreg(model, override.se = se, override.pvalues = p)
# look at effect of party call by foxnews

newdata <- CJ(FoxNews = 0:1, party_call = 0:1, s_daystoelection = seq(-2, 2, .25),
  Retirement = 0,
  seniorit = fox_news[, mean(seniorit)],
  voteshare_lag = fox_news[, mean(voteshare_lag)],
  qualchal_lag = 1,
  qualchal = 1,
  spendgap_lag = fox_news[, mean(spendgap_lag)],
  spendgap = fox_news[, mean(spendgap)],
  distpart_lag = fox_news[, mean(distpart_lag)],
  RegPass = 1,
  Susp = 0,
  OtherPass= 0,
  Amend = 0,
  ProPart = 0)
newdata[, yhat := predict(model, newdata)]
newdata[FoxNews == 1 & party_call == 1, group := "Fox News-party call"]
newdata[FoxNews == 1 & party_call == 0, group := "Fox News-non call"]
newdata[FoxNews == 0 & party_call == 1, group := "no Fox News-party call"]
newdata[FoxNews == 0 & party_call == 0, group := "no Fox News-non call"]
ggplot(newdata, aes(s_daystoelection, plogis(yhat),
  group = group, color = factor(party_call), linetype = factor(FoxNews))) +
  geom_line() + ylim(.25, 1)
