library(partycalls)
library(ggplot2)
library(extrafont)
library(Cairo)
loadfonts()

#------------------------------------------------------------------------------#
# Load data
#------------------------------------------------------------------------------#
load("test_data/new_whoheeds13_lm.RData")
house_data <- new_whoheeds13[drop == 0, ]
load("test_data/senate_data_lm.RData")
load("test_data/house_party_calls_lm.RData")
names(house_party_calls) <- paste0("hou", 93:112)
load("test_data/senate_party_calls_lm.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

#------------------------------------------------------------------------------#
# Cleaning (move this to another script)
#------------------------------------------------------------------------------#
house_data[is.na(vote_share), vote_share := 100]
setnames(house_data,
  c("bestgrosswart", "power"),
  c("best_committee", "power_committee"))
old_names <- c("pirate100", "pfrate100", "afam")
new_names <- c("responsiveness_to_party_calls", "baseline_rate",
  "african_american")
setnames(house_data, old_names, new_names)

senate_data <- senate_data[drop == 0, ]
senate_data[, dem := NA_real_]
senate_data[caucus == "Democrat", dem := 1]
senate_data[caucus == "Republican", dem := 0]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]
setnames(senate_data,
  c("maj"),
  c("majority"))
setnames(senate_data, old_names, new_names)
senate_data[congress == 107, majority := dem]

# drop Jeffords from 107?
senate_data <- senate_data[!(congress == 107 & icpsrLegis %in% c(14240, 94240))]

house_coding_record <- data.table(congress = 93:112)
house_coding_record[, party_calls := sapply(93:112, function(x)
  length(get_party_calls(house_party_calls[[paste0("hou", x)]])))]
house_coding_record[, noncalls := sapply(93:112, function(x)
  length(get_noncalls(house_party_calls[[paste0("hou", x)]])))]
house_coding_record[, gray_vote_count := sapply(93:112, function(x)
  length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))]
house_coding_record[, percent_party_calls :=
    100 * party_calls / (party_calls + noncalls)]
house_coding_record[, majority := "Republican"]
house_coding_record[congress %in% c(93:103, 110:111), majority := "Democrat"]

senate_coding_record <- data.table(congress = 93:112)
senate_coding_record[, party_calls := sapply(93:112, function(x)
  length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, noncalls := sapply(93:112, function(x)
  length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, gray_vote_count := sapply(93:112, function(x)
  length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, percent_party_calls :=
    100 * party_calls / (party_calls + noncalls)]
senate_coding_record[, majority := "Republican"]
senate_coding_record[congress %in% c(93:96, 100:103, 107, 110:112),
  majority := "Democrat"]

house_coding_record[, chamber := "House"]
senate_coding_record[, chamber := "Senate"]
coding_record <- rbind(house_coding_record, senate_coding_record)

#------------------------------------------------------------------------------#
# models
#------------------------------------------------------------------------------#
formula1 <- responsiveness_to_party_calls ~
  ideological_extremism + baseline_rate +
  vote_share + pres_vote_share + leader +  chair + power_committee +
  best_committee + female + african_american + latino + south +
  seniority + freshman
formula2 <- update.formula(formula1, . ~ . + up_for_reelection)

#------------------------------------------------------------------------------#
# tab-responsiveness-regressions
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

#------------------------------------------------------------------------------#
# tab-house-models
#------------------------------------------------------------------------------#
models <- list(
  lm(formula1, house_data),
  lm(formula1, house_data[dem == 1]),
  lm(formula1, house_data[dem == 0]),
  lm(formula1, house_data[majority == 1]),
  lm(update.formula(formula1, . ~ . - chair), house_data[majority == 0]))
ses <- list(
  robust_se(models[[1]],
    house_data[, congress],
    house_data[, icpsrLegis]),
  robust_se(models[[2]],
    house_data[dem == 1, congress],
    house_data[dem == 1, icpsrLegis]),
  robust_se(models[[3]],
    house_data[dem == 0, congress],
    house_data[dem == 0, icpsrLegis]),
  robust_se(models[[4]],
    house_data[majority == 1, congress],
    house_data[majority == 1, icpsrLegis]),
  robust_se(models[[5]],
    house_data[majority == 0, congress],
    house_data[majority == 0, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2:15, 1))

#------------------------------------------------------------------------------#
# tab-senate-models
#------------------------------------------------------------------------------#
models <- list(
  lm(formula2, senate_data),
  lm(formula2, senate_data[dem == 1]),
  lm(formula2, senate_data[dem == 0]),
  lm(formula2, senate_data[majority == 1]),
  lm(update.formula(formula2, . ~ . - chair), senate_data[majority == 0]))
ses <- list(
  robust_se(models[[1]],
    senate_data[, congress],
    senate_data[, icpsrLegis]),
  robust_se(models[[2]],
    senate_data[dem == 1, congress],
    senate_data[dem == 1, icpsrLegis]),
  robust_se(models[[3]],
    senate_data[dem == 0, congress],
    senate_data[dem == 0, icpsrLegis]),
  robust_se(models[[4]],
    senate_data[majority == 1, congress],
    senate_data[majority == 1, icpsrLegis]),
  robust_se(models[[5]],
    senate_data[majority == 0, congress],
    senate_data[majority == 0, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2:3, 16, 4:15, 1))

#------------------------------------------------------------------------------#
# Figure 1
#------------------------------------------------------------------------------#
cairo_pdf(file = "drafts/party_call_percent_both.pdf",
  width = 6, height = 6)
ggplot(coding_record, aes(congress, percent_party_calls,
  color = as.factor(majority))) +
  ylim(30, 90) +
  xlab("Congress") +
  ylab("Percentage of Votes") +
  ggtitle("Frequency of Party Calls over Time") +
  geom_point(aes(shape = as.factor(majority), size = ifelse(majority == "Democrat", 3, 2.25))) +
  scale_shape_manual("Majority Party", values = c(16, 17), guide = FALSE) +
  scale_color_manual("Majority Party", values = c("blue3", "red3"), guide = FALSE) +
  scale_size_continuous(limits = c(2, 3)) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Linux Libertine"),
    legend.position = "none") +
  facet_wrap(~ chamber)
dev.off()


#------------------------------------------------------------------------------#
# Figure 2
#------------------------------------------------------------------------------#
house_results <- house_data[, as.list(summary(
    lm(formula1, data = .SD), vcov = sandwich::vcovHC(type = "HC1"))$
    coef["ideological_extremism", 1:2]),
  .(Congress = congress, majority)]
senate_results <- senate_data[, as.list(summary(
  lm(formula2, data = .SD), vcov = sandwich::vcovHC(type = "HC1"))$
    coef["ideological_extremism", 1:2]),
  .(Congress = congress, majority)]
house_results[, chamber := "House"]
senate_results[, chamber := "Senate"]
congress_by_congress_results <- rbind(house_results, senate_results)

setnames(congress_by_congress_results, "Std. Error", "SE")
congress_by_congress_results[, q025 := Estimate + qnorm(.025) * SE]
congress_by_congress_results[, q975 := Estimate + qnorm(.975) * SE]
congress_by_congress_results[, q250 := Estimate + qnorm(.25) * SE]
congress_by_congress_results[, q750 := Estimate + qnorm(.75) * SE]
congress_by_congress_results[, maj := ifelse(majority == 1, "Majority", "Minority")]
congress_by_congress_results[maj == "Majority", party := "Democrat"]
congress_by_congress_results[maj == "Minority", party := "Republican"]
congress_by_congress_results[maj == "Majority" & chamber == "House" &
    Congress %in% house_coding_record[majority == "Republican", congress],
  party := "Republican"]
congress_by_congress_results[maj == "Minority" & chamber == "House" &
    Congress %in% house_coding_record[majority == "Republican", congress],
  party := "Democrat"]
congress_by_congress_results[maj == "Majority" & chamber == "Senate" &
    Congress %in% senate_coding_record[majority == "Republican", congress],
  party := "Republican"]
congress_by_congress_results[maj == "Minority" & chamber == "Senate" &
    Congress %in% senate_coding_record[majority == "Republican", congress],
  party := "Democrat"]

cairo_pdf(file="drafts/both-chambers-figure2.pdf", ## RENAME
  width = 6, height = 6)
ggplot(congress_by_congress_results,
  aes(Congress, Estimate, color = party, shape = party)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_errorbar(aes(ymin = q025, ymax = q975), size = .5, width = 0) +
  geom_errorbar(aes(ymin = q250, ymax = q750), size = 1, width = 0) +
  geom_point(aes(size = ifelse(party == "Democrat", 2.35, 2.125))) +
  facet_grid(maj ~ chamber, scales = "free") +
  theme_minimal() +
  scale_color_manual(values = c("Democrat" = "blue3", "Republican" = "red3")) +
  scale_size_continuous(limits = c(2, 3)) +
  ylab("Coefficient on Ideological Extremism") +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Linux Libertine"),
    strip.text = element_text(size = 12),
    legend.position = "none")
dev.off()

#------------------------------------------------------------------------------#
# Make Reelection Data
#------------------------------------------------------------------------------#

reelection_data <- senate_data[!is.na(responsiveness_to_party_calls)]
setorder(reelection_data, stabb, icpsrLegis, congress)

# subset to cases with two senators, make indicator for mean treatment
reelection_data <- merge(reelection_data,
  reelection_data[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
reelection_data <- reelection_data[N == 2]
reelection_data[, mean_up_for_reelection := mean(up_for_reelection), .(stabb, congress)]

# add lagged ideological extremism
reelection_data <- merge(
  reelection_data,
  reelection_data[, .(congress = congress + 1,
    lag_congress = congress, icpsrLegis,
    lag_responsiveness_to_party_calls = responsiveness_to_party_calls,
    lag_baseline_rate = baseline_rate,
    lag_ideological_extremism = ideological_extremism)],
  by = c("congress", "icpsrLegis"),
  all.x = TRUE)

reelection_data[, more_senior :=
    ifelse(rep(sd(seniority) == 0, 2), 0, as.numeric(seniority == max(seniority))),
  .(stabb, congress)]
reelection_data[, mean_more_senior := mean(more_senior), .(stabb, congress)]
reelection_data[, stabb_congress := paste(stabb, congress)]

#------------------------------------------------------------------------------#
# tab-reelection
#------------------------------------------------------------------------------#
models <- list(
  lfe::felm(responsiveness_to_party_calls ~ up_for_reelection |
      stabb_congress | 0 | icpsrLegis + congress,
    DATA[mean_up_for_reelection == .5]),
  lfe::felm(baseline_rate ~ up_for_reelection |
      stabb_congress | 0 | icpsrLegis + congress,
    DATA[mean_up_for_reelection == .5]),
  lfe::felm(responsiveness_to_party_calls ~ up_for_reelection +
      lag_responsiveness_to_party_calls +
      lag_ideological_extremism +
      lag_baseline_rate +
      caucus + majority +
      vote_share + pres_vote_share + leader +  chair + power_committee +
      best_committee + female + african_american + latino +
      seniority |
      stabb_congress | 0 | icpsrLegis + congress,
    DATA[mean_up_for_reelection == .5]),
  lfe::felm(baseline_rate ~ up_for_reelection +
      lag_responsiveness_to_party_calls +
      lag_ideological_extremism +
      lag_baseline_rate +
      lag_ideological_extremism +
      caucus + majority +
      vote_share + pres_vote_share + leader +  chair + power_committee +
      best_committee + female + african_american + latino +
      seniority |
      stabb_congress | 0 | icpsrLegis + congress,
    DATA[mean_up_for_reelection == .5])
)
texreg::texreg(models,
  custom.model.names = rep(c("Responsiveness", "Baseline Rate"), 2),
  custom.coef.names = fix_coef_names(models))

#------------------------------------------------------------------------------#
# Figure 3
#------------------------------------------------------------------------------#
differences <- data.table(
  test = c(
    "Party Calls",
    "Party-Free Votes"),
  Estimate = c(
    coef(models[[3]])["up_for_reelection"],
    coef(models[[4]])["up_for_reelection"]),
  SE = c(
    vcov(models[[3]])["up_for_reelection", "up_for_reelection"] ^ .5,
    vcov(models[[4]])["up_for_reelection", "up_for_reelection"] ^ .5))

differences[, Lower_Bound := Estimate + qnorm(.025) * SE]
differences[, Upper_Bound := Estimate + qnorm(.975) * SE]
differences[, Lower_50 := Estimate + qnorm(.25) * SE]
differences[, Upper_50 := Estimate + qnorm(.75) * SE]
differences[, test := factor(test, levels = test)]

cairo_pdf(file="drafts/senate_difference_estimates.pdf",
  width = 5, height = 4)
ggplot(differences, aes(test, Estimate)) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), width = 0, size = .75) +
  geom_errorbar(aes(ymin = Lower_50, ymax = Upper_50), width = 0, size = 1.5) +
  geom_point(size = 4) +
  xlab("") + ylab("Reelection Difference in Same-State Senators") +
  ggtitle(paste0("Reelection Limits Responsiveness to Party Calls")) +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Linux Libertine", size = 12))
dev.off()
