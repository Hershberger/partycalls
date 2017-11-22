library(partycalls)
library(data.table)
library(ggplot2)
library(extrafont)
library(Cairo)
loadfonts()

#------------------------------------------------------------------------------#
# model formulas
#------------------------------------------------------------------------------#
formula1 <- responsiveness_to_party_calls ~ ideological_extremism +
  baseline_rate + vote_share + pres_vote_share + leader + chair +
  power_committee + best_committee + female + african_american + latino +
  south + seniority + freshman
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
# Figure 1
#------------------------------------------------------------------------------#
cairo_pdf(file = "draft/party-calls-over-time.pdf",
  width = 6.5, height = 4)
ggplot(coding_record, aes(congress, percent_party_calls,
  color = as.factor(majority))) +
  ylim(30, 90) +
  xlab("Congress") +
  ylab("Percentage of Votes") +
  ggtitle("Frequency of Party Calls over Time") +
  geom_point(aes(shape = as.factor(majority),
    size = ifelse(majority == "Democrat", 2, 1.25))) +
  scale_shape_manual("Majority Party", values = c(16, 17), guide = FALSE) +
  scale_color_manual("Majority Party", values = c("blue3", "red3"), guide = FALSE) +
  scale_size_continuous(limits = c(1, 3)) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "gray", fill = NA),
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Linux Libertine"),
    strip.text = element_text(size = 12),
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

cairo_pdf(file = "draft/extremism-responsiveness.pdf", ## RENAME
  width = 6, height = 4)
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
reelection_data[, mean_up_for_reelection := mean(up_for_reelection),
  .(stabb, congress)]

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
    ifelse(rep(sd(seniority) == 0, 2), 0,
      as.numeric(seniority == max(seniority))),
  .(stabb, congress)]
reelection_data[, mean_more_senior := mean(more_senior), .(stabb, congress)]
reelection_data[, stabb_congress := paste(stabb, congress)]

#------------------------------------------------------------------------------#
# tab-reelection
#------------------------------------------------------------------------------#
models <- list(
  lfe::felm(responsiveness_to_party_calls ~ up_for_reelection |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5]),
  lfe::felm(baseline_rate ~ up_for_reelection |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5]),
  lfe::felm(responsiveness_to_party_calls ~ up_for_reelection +
      lag_responsiveness_to_party_calls +
      lag_ideological_extremism +
      lag_baseline_rate +
      caucus + majority +
      vote_share + pres_vote_share + leader +  chair + power_committee +
      best_committee + female + african_american + latino +
      seniority |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5]),
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
    reelection_data[mean_up_for_reelection == .5])
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

cairo_pdf(file="draft/senate_difference_estimates.pdf",
  width = 5, height = 4)
ggplot(differences, aes(test, Estimate)) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), width = 0,
    size = .75) +
  geom_errorbar(aes(ymin = Lower_50, ymax = Upper_50), width = 0, size = 1.5) +
  geom_point(size = 4) +
  xlab("") + ylab("Reelection Difference in Same-State Senators") +
  ggtitle(paste0("Reelection Limits Responsiveness to Party Calls")) +
  theme(
    panel.border = element_rect(fill = NA, color = "gray"),
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "Linux Libertine", size = 12))
dev.off()

#------------------------------------------------------------------------------#
# Supplemetal Appendix A: Vote Classification Analysis for Senate
#------------------------------------------------------------------------------#
# for the House
if (!file.exists("results/vote_coding_house_lm.RData")) {
  vote_coding_house_lm <- rbindlist(lapply(93:112, function(i) {
    cat("\r working on congress", i)
    out <- check_signs(house_party_calls[[paste0("hou", i)]])
    X <- house_party_calls[[paste0("hou", i)]]$voteMargins
    cbind(out, data.table(
      congress = i,
      party_call =
        house_party_calls[[paste0("hou", i)]]$party_call_coding$coding,
      close_vote = ifelse(
        X[, 4] / (X[, 1] + X[, 2]) <= .35,
        "lop", "close")
    ))
  }))

  save(vote_coding_house_lm, file = "results/vote_coding_house_lm.RData")
} else {
  load("results/vote_coding_house_lm.RData")
}

# for the senate
if (!file.exists("results/vote_coding_senate_lm.RData")) {
  vote_coding_senate_lm <- rbindlist(lapply(93:112, function(i) {
    cat("\r working on congress", i)
    out <- check_signs(senate_party_calls[[paste0("sen", i)]])
    X <- senate_party_calls[[paste0("sen", i)]]$voteMargins
    cbind(out, data.table(
      congress = i,
      party_call =
        senate_party_calls[[paste0("sen", i)]]$party_call_coding$coding,
      close_vote = ifelse(
        X[, 4] / (X[, 1] + X[, 2]) <= .35,
        "lop", "close")
    ))
  }))
  save(vote_coding_senate_lm, file = "results/vote_coding_senate_lm.RData")
} else {
  load("results/vote_coding_senate_lm.RData")
}

# rates of "gray" votes
vote_coding_house_lm[, mean(party_call == "gray")]
vote_coding_senate_lm[, mean(party_call == "gray")]

# make tables
calc_percentages <- function(tab) {
  perc_tab <- paste0("$(", sprintf("%2.0f", 100 * tab / sum(tab)), "\\%)$")
  tab[1, 1] <- paste0(" $", tab[1, 1], "$ ", perc_tab[1])
  tab[2, 1] <- paste0(" $", tab[2, 1], "$ ", perc_tab[2])
  tab[1, 2] <- paste0(" $", tab[1, 2], "$ ", perc_tab[3])
  tab[2, 2] <- paste0(" $", tab[2, 2], "$ ", perc_tab[4])
  unname(tab)
}

# tab-close-lop
cat(paste0(apply(
  cbind(
    vote_coding_house_lm[party_call != "gray",
      calc_percentages(table(close_vote, party_call))],
    vote_coding_senate_lm[party_call != "gray",
      calc_percentages(table(close_vote, party_call))]
  ), 1, paste0, collapse = "&"), "\\\\\n"))

# tab-sorting
cat(paste0(apply(
  rbind(
    cbind(
      c("($-$) Party ", "($+$) Party "),
      vote_coding_house_lm[party_call %in% c("party call", "noncall"),
        calc_percentages(table(party_coef, ideal_coef))],
      vote_coding_senate_lm[party_call %in% c("party call", "noncall"),
        calc_percentages(table(party_coef, ideal_coef))]
    ),
    cbind(
      c("($-$) Party ", "($+$) Party "),
      vote_coding_house_lm[party_call == "party call",
        calc_percentages(table(party_coef, ideal_coef))],
      vote_coding_senate_lm[party_call == "party call",
        calc_percentages(table(party_coef, ideal_coef))]
    ),
    cbind(
      c("($-$) Party ", "($+$) Party "),
      vote_coding_house_lm[party_call == "noncall",
        calc_percentages(table(party_coef, ideal_coef))],
      vote_coding_senate_lm[party_call == "noncall",
        calc_percentages(table(party_coef, ideal_coef))]
    )
  ), 1, paste0, collapse = "&"), "\\\\\n"))




#------------------------------------------------------------------------------#
# Supplemetal Appendix B: Summary Stats
#------------------------------------------------------------------------------#

summarize <- function(x, fmt) {
  paste0(paste0(c(
    Mean = sprintf(fmt, mean(x)),
    SD = sprintf(fmt, sd(x)),
    Min = sprintf(fmt, min(x)),
    Max = sprintf(fmt, max(x))), collapse = "$&$"), "\\\\\n")
}
# tab-senate-summary-stats
senate_stats <- c(
  senate_data[, summarize(responsiveness_to_party_calls, "%2.1f")],
  senate_data[, summarize(party_free_ideal_point, "%2.2f")],
  senate_data[, summarize(ideological_extremism, "%2.2f")],
  senate_data[, summarize(baseline_rate, "%2.1f")],
  senate_data[, summarize(up_for_reelection, "%2.2f")],
  senate_data[, summarize(vote_share, "%2.2f")],
  senate_data[, summarize(pres_vote_share, "%2.2f")],
  senate_data[, summarize(leader, "%2.2f")],
  senate_data[, summarize(chair, "%2.2f")],
  senate_data[, summarize(power_committee, "%2.2f")],
  senate_data[, summarize(best_committee, "%2.1f")],
  senate_data[, summarize(female, "%2.2f")],
  senate_data[, summarize(african_american, "%2.2f")],
  senate_data[, summarize(latino, "%2.2f")],
  senate_data[, summarize(south, "%2.2f")],
  senate_data[, summarize(seniority, "%2.2f")],
  senate_data[, summarize(freshman, "%2.2f")]
)

# tab-house-summary-stats
house_stats <- c(
  house_data[, summarize(responsiveness_to_party_calls, "%2.1f")],
  house_data[, summarize(party_free_ideal_point, "%2.2f")],
  house_data[, summarize(ideological_extremism, "%2.2f")],
  house_data[, summarize(baseline_rate, "%2.1f")],
  house_data[, summarize(vote_share, "%2.2f")],
  house_data[, summarize(pres_vote_share, "%2.2f")],
  house_data[, summarize(leader, "%2.2f")],
  house_data[, summarize(chair, "%2.2f")],
  house_data[, summarize(power_committee, "%2.2f")],
  house_data[, summarize(best_committee, "%2.1f")],
  house_data[, summarize(female, "%2.2f")],
  house_data[, summarize(african_american, "%2.2f")],
  house_data[, summarize(latino, "%2.2f")],
  house_data[, summarize(south, "%2.2f")],
  house_data[, summarize(seniority, "%2.2f")],
  house_data[, summarize(freshman, "%2.2f")]
)

#------------------------------------------------------------------------------#
# Supplemetal Appendix C
#------------------------------------------------------------------------------#
# tab-house-models
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

# tab-senate-models
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
# Supplemetal Appendix E
#------------------------------------------------------------------------------#

# compare responsiveness to party calls with party unity scores
house_party_vote_rate_data <- rbindlist(lapply(1:20,
  calc_party_vote_rates, house_party_calls))
senate_party_vote_rate_data <- rbindlist(lapply(1:20,
  calc_party_vote_rates, senate_party_calls))
house_vote_with_party_rate_data <- rbindlist(lapply(1:20,
  calc_vote_with_party_rates, house_party_calls))
senate_vote_with_party_rate_data <- rbindlist(lapply(1:20,
  calc_vote_with_party_rates, senate_party_calls))
house_vote_with_nonparty_rate_data <- rbindlist(lapply(1:20,
  calc_nonparty_vote_rates, house_party_calls))
senate_vote_with_nonparty_rate_data <- rbindlist(lapply(1:20,
  calc_nonparty_vote_rates, senate_party_calls))
house_rate_data <- merge(
  house_data,
  merge(
    merge(house_party_vote_rate_data, house_vote_with_party_rate_data,
      by = c("congress", "icpsrLegis")),
    house_vote_with_nonparty_rate_data,
    by = c("congress", "icpsrLegis"))
  )
senate_rate_data <- merge(
  senate_data,
  merge(
    merge(senate_party_vote_rate_data, senate_vote_with_party_rate_data,
      by = c("congress", "icpsrLegis")),
    senate_vote_with_nonparty_rate_data,
    by = c("congress", "icpsrLegis"))
)

# similarity between party vote coding and party call coding
calc_similarity_party_vote_party_call <- function(congress_index, rollcall_list,
  chamber)
{
  rc <- rollcall_list[[congress_index]]
  votes <- rc$votes
  votes <- melt(votes)
  data.table::setDT(votes)
  setnames(votes, c("mc", "vote_id", "vote"))
  party_call_coding <- rc$party_call_coding$coding
  party_vote_coding <- rep("non-party vote", rc$m)
  party_vote_coding[identify_party_votes(congress_index, rollcall_list)] <-
    "party vote"
  table(party_call_coding, party_vote_coding)
  data.table(congress = congress_index + 92, chamber,
    similarity_rate = mean(
      (party_call_coding == "noncall" & party_vote_coding == "non-party vote") |
        (party_call_coding == "party call" & party_vote_coding == "party vote")))

}

coding_record <- merge(coding_record,
  rbind(
    rbindlist(lapply(1:20, calc_similarity_party_vote_party_call,
      house_party_calls, "House")),
    rbindlist(lapply(1:20, calc_similarity_party_vote_party_call,
      senate_party_calls, "Senate"))),
  by = c("congress", "chamber"))


# correlations reported in Supplemental Appendix E
house_rate_data[, cor(cbind(
  responsiveness_to_party_calls, baseline_rate,
  party_vote_rate,
  nonparty_vote_rate,
  vote_with_party_rate), use = "p", method = "s")]
house_rate_data[, cor(
  responsiveness_to_party_calls, vote_with_party_rate, use = "p", method = "s"),
  .(congress)]
senate_rate_data[, cor(cbind(
  responsiveness_to_party_calls, baseline_rate,
  party_vote_rate,
  nonparty_vote_rate,
  vote_with_party_rate), use = "p", method = "s")]
senate_rate_data[, cor(
  responsiveness_to_party_calls, vote_with_party_rate, use = "p", method = "s"),
  .(congress)]

#------------------------------------------------------------------------------#
# tab-party-unity-regressions
#------------------------------------------------------------------------------#

formula1 <- party_vote_rate ~ ideological_extremism +
  nonparty_vote_rate + vote_share + pres_vote_share + leader + chair +
  power_committee + best_committee + female + african_american + latino +
  south + seniority + freshman
formula2 <- update.formula(formula1, . ~ . + up_for_reelection)
models <- list(
  lm(formula1, house_rate_data),
  lm(formula1, senate_rate_data),
  lm(formula2, senate_rate_data))
ses <- list(
  robust_se(models[[1]],
    house_rate_data[!is.na(party_vote_rate) & !is.na(nonparty_vote_rate), congress],
    house_rate_data[!is.na(party_vote_rate) & !is.na(nonparty_vote_rate), icpsrLegis]),
  robust_se(models[[2]],
    senate_rate_data[!is.na(party_vote_rate) & !is.na(nonparty_vote_rate), congress],
    senate_rate_data[!is.na(party_vote_rate) & !is.na(nonparty_vote_rate), icpsrLegis]),
  robust_se(models[[3]],
    senate_rate_data[!is.na(party_vote_rate) & !is.na(nonparty_vote_rate), congress],
    senate_rate_data[!is.na(party_vote_rate) & !is.na(nonparty_vote_rate), icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2, 3, 16, 4:15, 1))


#------------------------------------------------------------------------------#
# tab-party-unity-up-for-reelection
#------------------------------------------------------------------------------#

reelection_data <- senate_rate_data[!is.na(responsiveness_to_party_calls)]
setorder(reelection_data, stabb, icpsrLegis, congress)

# subset to cases with two senators, make indicator for mean treatment
reelection_data <- merge(reelection_data,
  reelection_data[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
reelection_data <- reelection_data[N == 2]
reelection_data[, mean_up_for_reelection := mean(up_for_reelection),
  .(stabb, congress)]

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
    ifelse(rep(sd(seniority) == 0, 2), 0,
      as.numeric(seniority == max(seniority))),
  .(stabb, congress)]
reelection_data[, mean_more_senior := mean(more_senior), .(stabb, congress)]
reelection_data[, stabb_congress := paste(stabb, congress)]


# add party vote rate and non-party-vote unity
reelection_data <- merge(
  reelection_data,
  reelection_data[, .(congress = congress + 1,
    lag_congress = congress, icpsrLegis,
    lag_party_vote_rate = party_vote_rate,
    lag_nonparty_vote_rate = nonparty_vote_rate)],
  by = c("congress", "icpsrLegis"),
  all.x = TRUE)
models <- list(
  lfe::felm(party_vote_rate ~ up_for_reelection |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5]),
  lfe::felm(nonparty_vote_rate ~ up_for_reelection |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5]),
  lfe::felm(party_vote_rate ~ up_for_reelection +
      lag_party_vote_rate +
      lag_ideological_extremism +
      lag_nonparty_vote_rate +
      caucus + majority +
      vote_share + pres_vote_share + leader +  chair + power_committee +
      best_committee + female + african_american + latino +
      seniority |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5]),
  lfe::felm(nonparty_vote_rate ~ up_for_reelection +
      lag_party_vote_rate +
      lag_ideological_extremism +
      lag_nonparty_vote_rate +
      caucus + majority +
      vote_share + pres_vote_share + leader +  chair + power_committee +
      best_committee + female + african_american + latino +
      seniority |
      stabb_congress | 0 | icpsrLegis + congress,
    reelection_data[mean_up_for_reelection == .5])
)
texreg::texreg(models,
  custom.model.names = rep(c("Party Vote Unity", "Non-Party Vote Unity"), 2),
  custom.coef.names = fix_coef_names(models))

