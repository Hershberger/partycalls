library(partycalls)
load("results/house_party_calls_brglm.RData")
load("results/senate_party_calls_brglm.RData")

house_responsiveness_brglm <- rbindlist(lapply(93:112, function(congress) {
  cat("\r", congress)
  rc <- make_member_year_data(congress, house_party_calls_brglm)
  DATA <- rc$member_year_data
  DATA[, .(congress,
    icpsrLegis,
    party_free_ideal_point_brglm = pf_ideal,
    responsiveness_to_party_calls_brglm = 100 * responsiveness_party_calls,
    baseline_rate_brglm = 100 * responsiveness_noncalls,
    ideological_extremism_brglm = ideological_extremism)]
}))
house_data <- merge(house_data, house_responsiveness_brglm, all.x = TRUE,
  by = c("congress", "icpsrLegis"))
house_data[dem == 1 & ideological_extremism_brglm != -party_free_ideal_point_brglm,
  ideological_extremism_brglm := -party_free_ideal_point_brglm]
house_data[dem == 0 & ideological_extremism_brglm != party_free_ideal_point_brglm,
  ideological_extremism_brglm := party_free_ideal_point_brglm]


senate_responsiveness_brglm <- rbindlist(lapply(93:112, function(congress) {
  cat("\r", congress)
  rc <- make_member_year_data(congress, senate_party_calls_brglm, chamber = "senate")
  DATA <- rc$member_year_data
  DATA[, .(congress,
    icpsrLegis,
    party_free_ideal_point_brglm = pf_ideal,
    responsiveness_to_party_calls_brglm = 100 * responsiveness_party_calls,
    baseline_rate_brglm = 100 * responsiveness_noncalls,
    ideological_extremism_brglm = ideological_extremism)]
}))
senate_data <- merge(senate_data, senate_responsiveness_brglm, all.x = TRUE,
  by = c("congress", "icpsrLegis"))
senate_data[dem == 1 & ideological_extremism_brglm != -party_free_ideal_point_brglm,
  ideological_extremism_brglm := -party_free_ideal_point_brglm]
senate_data[dem == 0 & ideological_extremism_brglm != party_free_ideal_point_brglm,
  ideological_extremism_brglm := party_free_ideal_point_brglm]
senate_data <- senate_data[!is.na(responsiveness_to_party_calls_brglm)]

#------------------------------------------------------------------------------#
# model formulas
#------------------------------------------------------------------------------#
formula1 <- responsiveness_to_party_calls_brglm ~ ideological_extremism_brglm +
  baseline_rate_brglm + vote_share + pres_vote_share + leader + chair +
  power_committee + best_committee + female + african_american + latino +
  south + seniority + freshman
formula2 <- update.formula(formula1, . ~ . + up_for_reelection)

#------------------------------------------------------------------------------#
# compare to tab-responsiveness-regressions
#------------------------------------------------------------------------------#
models <- list(
  lm(formula1, house_data),
  lm(formula1, senate_data),
  lm(formula2, senate_data))
ses <- list(
  robust_se(models[[1]],
    house_data[, congress],
    house_data[, icpsrLegis]),
  robust_se(models[[2]],
    senate_data[, congress],
    senate_data[, icpsrLegis]),
  robust_se(models[[3]],
    senate_data[, congress],
    senate_data[, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2, 3, 16, 4:15, 1))
