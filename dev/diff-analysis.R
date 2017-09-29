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
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2, 3, 11, 4:10, 1)
  )


#------------------------------------------------------------------------------#
# Figure 2
#------------------------------------------------------------------------------#
house_results <- house_diff_data[, as.list(summary(
  lm(formula1, data = .SD), vcov = sandwich::vcovHC(type = "HC1"))$
    coef["ideological_extremism_diff", 1:2]),
  .(Congress = congress, majority)]
senate_results <- senate_diff_data[, as.list(summary(
  lm(formula2, data = .SD), vcov = sandwich::vcovHC(type = "HC1"))$
    coef["ideological_extremism_diff", 1:2]),
  .(Congress = congress, majority)]
house_results[, chamber := "House"]
senate_results[, chamber := "Senate"]
congress_by_congress_results <- rbind(house_results, senate_results)

setnames(congress_by_congress_results, "Std. Error", "SE")
congress_by_congress_results[, q025 := Estimate + qnorm(.025) * SE]
congress_by_congress_results[, q975 := Estimate + qnorm(.975) * SE]
congress_by_congress_results[, q250 := Estimate + qnorm(.25) * SE]
congress_by_congress_results[, q750 := Estimate + qnorm(.75) * SE]
congress_by_congress_results[,
  maj := ifelse(majority == 1, "Majority", "Minority")]
congress_by_congress_results[maj == "Majority", party := "Democrat"]
congress_by_congress_results[maj == "Minority", party := "Republican"]
congress_by_congress_results[maj == "Majority" & chamber == "House" &
    Congress %in% coding_record[chamber == "House" & majority == "Republican",
      congress],
  party := "Republican"]
congress_by_congress_results[maj == "Minority" & chamber == "House" &
    Congress %in% coding_record[chamber == "House" & majority == "Republican",
      congress],
  party := "Democrat"]
congress_by_congress_results[maj == "Majority" & chamber == "Senate" &
    Congress %in% coding_record[chamber == "Senate" & majority == "Republican",
      congress],
  party := "Republican"]
congress_by_congress_results[maj == "Minority" & chamber == "Senate" &
    Congress %in% coding_record[chamber == "Senate" & majority == "Republican",
      congress],
  party := "Democrat"]


ggplot(congress_by_congress_results,
  aes(Congress, Estimate, color = party, shape = party)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_errorbar(aes(ymin = q025, ymax = q975), size = .5, width = 0) +
  geom_errorbar(aes(ymin = q250, ymax = q750), size = 1, width = 0) +
  geom_point(aes(size = ifelse(party == "Democrat", 2.35, 2.125))) +
  facet_grid(maj ~ chamber, scales = "free") +
  theme_minimal() +
  scale_color_manual(values = c("Democrat" = "blue3", "Republican" = "red3")) +
  scale_size_continuous(limits = c(2, 4)) +
  ylab("Coefficient on Ideological Extremism") +
  ggtitle("Ideological Extremists Are More Responsive to Party Calls") +
  theme(
    panel.border = element_rect(fill = NA, color = "gray"),
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Linux Libertine"),
    strip.text = element_text(size = 12),
    legend.position = "none")
