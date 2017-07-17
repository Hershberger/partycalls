library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
data <- senator_year_data[drop == 0]
data[, id := as.numeric(factor(icpsrLegis))]
data[, time := as.numeric(factor(congress))]
data[, party_time := as.numeric(factor((caucus == "Democrat") * 100 + congress))]
data[is.na(power_committee), power_committee:=0]
data[is.na(best_committee), best_committee:= 0]
data_list <- list(
  N = nrow(data), #I = max(data$id),
  J = max(data$party_time),
  #i = data$id,
  jj = data$party_time,
  y = data$responsiveness_party_calls,
  X_f = data[, .(
    arm::rescale(pres_vote_share),
    arm::rescale(vote_share),
    arm::rescale(pres_vote_share) * arm::rescale(vote_share),
    # arm::rescale(best_committee),
    # arm::rescale(seniority),
    # leader, chair, power_committee, freshman, superfreshman,
    # retiree, south17,
    # gingrich_senator,
    up_for_reelection
    )],
  X_r = data[, .(
    arm::rescale(ideological_extremism)
  )]
)
data_list$K_f <- ncol(data_list$X_f)
data_list$K_r <- ncol(data_list$X_r)

sm <- stan_model("dev/model.stan")
sf <- vb(sm, data_list, iter = 20000)
ex <- extract(sf)
coef_R <- mean(ex$a) + colMeans(ex$a_T)[1:20]
coef_D <- mean(ex$a) + colMeans(ex$a_T)[21:40]
ggplot(data.table(coef = c(coef_R, coef_D), congress = rep(93:112, 2),
  party = rep(c("R", "D"), each = 20)), aes(congress, coef, color = party)) +
  geom_point() + geom_smooth(se = FALSE)

