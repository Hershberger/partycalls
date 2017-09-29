library(partycalls)
library(ggplot2)
library(extrafont)
library(Cairo)
loadfonts()

#------------------------------------------------------------------------------#
# model formulas
#------------------------------------------------------------------------------#
formula1 <- responsiveness_to_party_calls_diff ~ ideological_extremism_diff +
  baseline_rate_diff + vote_share_diff + pres_vote_share_diff + leader_diff +
  chair_diff + power_committee_diff + best_committee_diff + freshman_diff
formula2 <- update.formula(formula1, . ~ . + up_for_reelection_diff)

house_diff_data <- merge(
  house_data[, .(icpsrLegis, congress, majority,
    responsiveness_to_party_calls, ideological_extremism, baseline_rate,
    vote_share, pres_vote_share, leader, chair, power_committee, best_committee,
    freshman)],
  house_data[, .(icpsrLegis, congress = congress + 1,
    responsiveness_to_party_calls_lag = responsiveness_to_party_calls,
    ideological_extremism_lag = ideological_extremism,
    baseline_rate_lag = baseline_rate,
    vote_share_lag = vote_share,
    pres_vote_share_lag = pres_vote_share,
    leader_lag = leader,
    chair_lag = chair,
    power_committee_lag = power_committee,
    best_committee_lag = best_committee,
    freshman_lag = freshman)],
  by = c("icpsrLegis", "congress"))[, .(icpsrLegis, congress, majority,
    responsiveness_to_party_calls_diff =
      responsiveness_to_party_calls - responsiveness_to_party_calls_lag,
    ideological_extremism_diff =
      ideological_extremism - ideological_extremism_lag,
    baseline_rate_diff = baseline_rate - baseline_rate_lag,
    vote_share_diff = vote_share - vote_share_lag,
    pres_vote_share_diff = pres_vote_share - pres_vote_share_lag,
    leader_diff = leader - leader_lag,
    chair_diff = chair - chair_lag,
    power_committee_diff = power_committee - power_committee_lag,
    best_committee_diff = best_committee - best_committee_lag,
    freshman_diff = freshman - freshman_lag
  )]

senate_diff_data <- merge(
  senate_data[, .(icpsrLegis, congress, majority,
    responsiveness_to_party_calls, ideological_extremism, baseline_rate,
    vote_share, pres_vote_share, leader, chair, power_committee, best_committee,
    freshman, up_for_reelection)],
  senate_data[, .(icpsrLegis, congress = congress + 1,
    responsiveness_to_party_calls_lag = responsiveness_to_party_calls,
    ideological_extremism_lag = ideological_extremism,
    baseline_rate_lag = baseline_rate,
    vote_share_lag = vote_share,
    pres_vote_share_lag = pres_vote_share,
    leader_lag = leader,
    chair_lag = chair,
    power_committee_lag = power_committee,
    best_committee_lag = best_committee,
    freshman_lag = freshman,
    up_for_reelection_lag = up_for_reelection)],
  by = c("icpsrLegis", "congress"))[, .(icpsrLegis, congress, majority,
    responsiveness_to_party_calls_diff =
      responsiveness_to_party_calls - responsiveness_to_party_calls_lag,
    ideological_extremism_diff =
      ideological_extremism - ideological_extremism_lag,
    baseline_rate_diff = baseline_rate - baseline_rate_lag,
    vote_share_diff = vote_share - vote_share_lag,
    pres_vote_share_diff = pres_vote_share - pres_vote_share_lag,
    leader_diff = leader - leader_lag,
    chair_diff = chair - chair_lag,
    power_committee_diff = power_committee - power_committee_lag,
    best_committee_diff = best_committee - best_committee_lag,
    freshman_diff = freshman - freshman_lag,
    up_for_reelection_diff = up_for_reelection - up_for_reelection_lag
  )]


#------------------------------------------------------------------------------#
# tab-responsiveness-diff-regressions
#------------------------------------------------------------------------------#
models <- list(
  lm(formula1, house_diff_data),
  lm(formula1, senate_diff_data),
  lm(formula2, senate_diff_data))
ses <- list(
  robust_se(models[[1]],
    house_diff_data[, congress],
    house_diff_data[, icpsrLegis]),
  robust_se(models[[2]],
    senate_diff_data[, congress],
    senate_diff_data[, icpsrLegis]),
  robust_se(models[[3]],
    senate_diff_data[, congress],
    senate_diff_data[, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::htmlreg(models, file = "tab-responsiveness-diff-regressions.doc",
  override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2, 3, 11, 4:10, 1)
  )



#------------------------------------------------------------------------------#
# tab-responsiveness-diff-regressions
#------------------------------------------------------------------------------#

formula1 <- responsiveness_to_party_calls ~ ideological_extremism +
  baseline_rate + vote_share + pres_vote_share + leader +
  chair + power_committee + best_committee + freshman |
  icpsrLegis | 0 | icpsrLegis
formula2 <- responsiveness_to_party_calls ~ ideological_extremism +
  baseline_rate + vote_share + pres_vote_share + leader +
  chair + power_committee + best_committee + freshman  +
  up_for_reelection |
  icpsrLegis | 0 | icpsrLegis
models <- list(
  lfe::felm(formula1, house_data),
  lfe::felm(formula1, senate_data),
  lfe::felm(formula2, senate_data))
ses <- list(
  robust_se(models[[1]],
    house_diff_data[, congress],
    house_diff_data[, icpsrLegis]),
  robust_se(models[[2]],
    senate_diff_data[, congress],
    senate_diff_data[, icpsrLegis]),
  robust_se(models[[3]],
    senate_diff_data[, congress],
    senate_diff_data[, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::htmlreg(models, file = "tab-responsiveness-diff-regressions.doc",
  override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2, 3, 11, 4:10, 1)
)
