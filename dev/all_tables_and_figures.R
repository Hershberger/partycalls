library(partycalls)
library(ggplot2)
library(gridExtra)
library(xtable)


# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]
load("test_data/house_party_calls_lm.RData")
names(house_party_calls) <- paste0("hou", 93:112)
load("test_data/senate_party_calls_lm.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

# subsets of the data
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


DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, y = pirate100 - pfrate100,
  y1 = pirate100, y2 = pfrate100)]
setorder(DATA, stabb, congress, class)
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, mean_tr := mean(tr), .(stabb, congress)]
DATA[, both_same_party := 1 * (length(unique(caucus)) == 1), .(stabb, congress)]
DATA[, both_democrats := 0]
DATA[, both_republicans := 0]
DATA[both_same_party == 1, both_democrats := 1 * (unique(caucus)[1] == "Democrat"),
  .(stabb, congress)]
DATA[both_same_party == 1, both_republicans := 1 * (unique(caucus)[1] == "Republican"),
  .(stabb, congress)]
DATA[, both_same_majority_status := 1 * (length(unique(maj)) == 1), .(stabb, congress)]
DATA[, both_majority := 0]
DATA[, both_minority := 0]
DATA[both_same_majority_status == 1, both_majority := 1 * (unique(maj)[1] == 1),
  .(stabb, congress)]
DATA[both_same_majority_status == 1, both_minority := 1 * (unique(maj)[1] == 0),
  .(stabb, congress)]
DATA[, majority_democrat := 1 * (maj == 1 & caucus == "Democrat")]
DATA[, majority_republican := 1 * (maj == 1 & caucus == "Republican")]
DATA[, split_majority_democrat := 0]
DATA[, split_majority_republican := 0]
DATA[both_same_party == 0 & both_same_majority_status == 0,
  split_majority_democrat := 1 * (max(majority_democrat) == 1),
  .(stabb, congress)]
DATA[both_same_party == 0 & both_same_majority_status == 0,
  split_majority_republican := 1 * (max(majority_republican) == 1),
  .(stabb, congress)]
DATA[both_majority == 1 & both_democrats == 1, seat_pair_type := "2 maj dems"]
DATA[both_majority == 1 & both_republicans == 1, seat_pair_type := "2 maj reps"]
DATA[both_minority == 1 & both_democrats == 1, seat_pair_type := "2 min dems"]
DATA[both_minority == 1 & both_republicans == 1, seat_pair_type := "2 min reps"]
DATA[split_majority_democrat == 1, seat_pair_type := "split/maj dem"]
DATA[split_majority_republican == 1, seat_pair_type := "split/maj rep"]
DATA[seat_pair_type == "split/maj dem" & caucus == "Democrat" & tr == 1,
  seat_pair_type := "split/maj dem, dem"]
DATA[seat_pair_type == "split/maj dem" & caucus == "Republican" & tr == 1,
  seat_pair_type := "split/maj dem, rep"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Democrat" & tr == 1,
  seat_pair_type := "split/maj rep, dem"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Republican" & tr == 1,
  seat_pair_type := "split/maj rep, rep"]
DATA[seat_pair_type == "split/maj dem" & caucus == "Democrat" & tr == 0,
  seat_pair_type := "split/maj dem, rep"]
DATA[seat_pair_type == "split/maj dem" & caucus == "Republican" & tr == 0,
  seat_pair_type := "split/maj dem, dem"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Democrat" & tr == 0,
  seat_pair_type := "split/maj rep, rep"]
DATA[seat_pair_type == "split/maj rep" & caucus == "Republican" & tr == 0,
  seat_pair_type := "split/maj rep, dem"]
DATA[, `:=`(both_same_party = NULL, both_democrats = NULL,
  both_republicans = NULL, both_same_majority_status = NULL,
  both_majority = NULL, both_minority = NULL, majority_democrat = NULL,
  majority_republican = NULL, split_majority_democrat = NULL,
  split_majority_republican = NULL)]


# formulas necessary for analysis
hou_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_votepct + south + votepct + female + afam + latino +
  seniority + freshman + bestgrosswart + leader +
  power + chair
sen_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share +
  female + afam + latino + up_for_reelection +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair


# functions necessary for analysis
test_rollcall2 <- function(.SD, type = c("brglm", "lm", "glm"))
{
  .SD <- .SD[party %in% c("D", "R")]
  n_yea_reps <- .SD[, sum(y == 1 & party == "R", na.rm = TRUE)]
  n_nay_reps <- .SD[, sum(y == 0 & party == "R", na.rm = TRUE)]
  n_yea_dems <- .SD[, sum(y == 1 & party == "D", na.rm = TRUE)]
  n_nay_dems <- .SD[, sum(y == 0 & party == "D", na.rm = TRUE)]
  party_line_vote <-
    (n_yea_reps == 0 & n_nay_reps >  0 & n_yea_dems >  0 & n_nay_dems == 0) |
    (n_yea_reps >  0 & n_nay_reps == 0 & n_yea_dems == 0 & n_nay_dems >  0)
  if (mean(.SD[, y], na.rm = TRUE) %in% c(0:1, NaN) |
      length(unique(.SD[!is.na(y) & party %in% c("D", "R"), party])) == 1L) {
    out <- list(b = 0, se = 0, t = Inf, p = NA_real_)
    out <- list(
      party_t = NA_real_,
      ideal_t = NA_real_)
  } else if (party_line_vote) {
    # out <- list(b = 1, se = 0, t = Inf, p = 0)
    m <- brglm::brglm(y ~ x, data = .SD, family = binomial)
    suppressWarnings(ideal_summ <- summary(m)$coef["x", ])
    out <- list(
      party_t = NA_real_,
      ideal_t = ideal_summ["z value"])
  } else {
    if (type == "brglm") {
      m <- brglm::brglm(y ~ republican + x, data = .SD, family = binomial)
      suppressWarnings(party_summ <- summary(m)$coef["republican", ])
      suppressWarnings(ideal_summ <- summary(m)$coef["x", ])
      out <- list(
        party_t = party_summ["z value"],
        ideal_t = ideal_summ["z value"])
    } else if (type == "glm") {
      suppressWarnings(m <- glm(y ~ republican + x, data = .SD, family = binomial))
      suppressWarnings(party_summ <- summary(m)$coef["republican", ])
      suppressWarnings(ideal_summ <- summary(m)$coef["x", ])
      out <- list(
        party_t = party_summ["z value"],
        ideal_t = ideal_summ["z value"])
    } else {
      m <- lm(y ~ republican + x, data = .SD)
      suppressWarnings(party_summ <- summary(m)$coef["republican", ])
      suppressWarnings(ideal_summ <- summary(m)$coef["x", ])
      out <- list(
        party_t = party_summ["t value"],
        ideal_t = ideal_summ["t value"])
    }
  }
  out$n_yea_reps <- n_yea_reps
  out$n_nay_reps <- n_nay_reps
  out$n_yea_dems <- n_yea_dems
  out$n_nay_dems <- n_nay_dems
  out$party_line_vote <- party_line_vote
  out
}

check_signs <- function(rc)
{
  n_iterations <- length(rc$record_of_ideals)

  ideal_dt <- merge(
    data.table(x = as.vector(rc$record_of_ideals[[n_iterations]]),
      mc = rownames(rc$record_of_ideals[[n_iterations]])),
    data.table(mc = rownames(rc$legis.data), party = rc$legis.data$party),
    by = "mc", all = TRUE)
  if (ideal_dt[party == "D", mean(x)] > ideal_dt[party == "R", mean(x)]) {
    ideal_dt[, x := -x]
  }

  DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  DT$y <- as.vector(rc$votes)
  # DT$party <- rc$legis.data$party
  DT[y %in% c(0, 9), y:= NA]
  DT[y == -1, y:= 0]
  DT <- merge(DT,
    ideal_dt,
    by = "mc", all = TRUE)
  DT[, republican := as.numeric(party == "R")]

  regs <- DT[party %in% c("D", "R"), test_rollcall2(.SD, type = "lm"), vt]
  levels <- c("negative", "positive")
  regs[, party_coef := "positive"]
  regs[party_t <= 0, party_coef := "negative"]
  regs[, ideal_coef := "positive"]
  regs[ideal_t <= 0, ideal_coef := "negative"]
  regs
}


# # Main Paper

# Make Table 1
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
new_whoheeds13[is.na(vote_share), vote_share := 100]
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]

setnames(new_whoheeds13, c("bestgrosswart", "power"),
  c("best_committee", "power_committee"))

hou_extremism <- pirate100 ~ ideological_extremism +  pfrate100 + vote_share +
  pres_vote_share + leader +  chair + power_committee + best_committee +
  female + afam + latino + south + seniority + freshman

sen_extremism <- pirate100 ~ ideological_extremism +  pfrate100 + vote_share +
  pres_vote_share + leader +  chair + power_committee + best_committee +
  female + afam + latino + south + seniority + freshman + up_for_reelection

texreg::screenreg(list(lm(hou_extremism, new_whoheeds13),
  lm(hou_extremism, senate_data),
  lm(sen_extremism, senate_data)),
  reorder.coef = c(2:16, 1))

texreg::texreg(list(lm(hou_extremism, new_whoheeds13),
  lm(hou_extremism, senate_data),
  lm(sen_extremism, senate_data)),
  reorder.coef = c(2:16, 1))

texreg::texreg(lm(sen_extremism2, senate_data))

# Make Figure 1
hou_record_plot <- ggplot(house_coding_record, aes(congress, percent_party_calls,
  color = as.factor(majority))) +
  ylim(30, 90) +
  xlab("Congress") +
  ylab("Party Call Percent") +
  labs(title = "U.S. House of Represenatives") +
  geom_point(aes(shape = as.factor(majority))) +
  scale_shape_manual("Majority Party",values = c(16, 17), guide = FALSE) +
  scale_color_manual("Majority Party",values = c("blue3", "red3"), guide = FALSE) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_classic()
sen_record_plot <- ggplot(senate_coding_record, aes(congress, percent_party_calls,
  color = as.factor(majority))) +
  ylim(30, 90) +
  xlab("Congress") +
  ylab("") +
  labs(title = "U.S. Senate") +
  geom_point(aes(shape = as.factor(majority))) +
  scale_shape_manual("Majority Party",values = c(16, 17), guide = FALSE) +
  scale_color_manual("Majority Party",values = c("blue3", "red3"), guide = FALSE) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_classic()
fig3 <- arrangeGrob(hou_record_plot, sen_record_plot, ncol = 2)
ggsave("plots/party_call_percent_both.pdf", fig3)

# Make Figure 2
# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  votes, tr = up_for_reelection, y1 = pirate100, y2 = pfrate100)]
setorder(DATA, stabb, congress, class)

# make dems majority for congress 107
DATA[congress == 107 & caucus == "Democrat", maj := 1]
DATA[congress == 107 & caucus == "Republican", maj := 0]

# subset to cases with two senators, one treated, one control
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, mean_tr := mean(tr), .(stabb, congress)]

# Estimate Effects
diff_pi <- DATA[mean_tr == .5,
  sum(tr * y1) - sum((1 - tr) * y1), .(stabb, congress)][,
    mean(V1)]
diff_pf <- DATA[mean_tr == .5,
  sum(tr * y2) - sum((1 - tr) * y2), .(stabb, congress)][,
    mean(V1)]

# Do inference
# bootstrap by state
states <- DATA[, unique(stabb)]

boot <- function(i) {
  boot_states <- sample(states, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- DATA[stabb == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_diff_pi <- boot_DATA[mean_tr == .5,
    sum(tr * y1) - sum((1 - tr) * y1), .(boot_id, congress)][,
      mean(V1)]
  boot_diff_pf <- boot_DATA[mean_tr == .5,
    sum(tr * y2) - sum((1 - tr) * y2), .(boot_id, congress)][,
      mean(V1)]
  data.table(boot_diff_pi, boot_diff_pf)
}

boots <- rbindlist(lapply(1:1000, boot))
differences <- data.table(test = c("Party Call Difference",
  "Party Free Difference"),
  Estimate = c(diff_pi, diff_pf),
  Lower_Bound = c(boots[, quantile(boot_diff_pi, .025)],
    boots[, quantile(boot_diff_pf, .025)]),
  Upper_Bound = c(boots[, quantile(boot_diff_pi, .975)],
    boots[, quantile(boot_diff_pf, .975)]),
  Lower_50 = c(boots[, quantile(boot_diff_pi, 0.25)],
    boots[, quantile(boot_diff_pf, 0.25)]),
  Upper_50 = c(boots[, quantile(boot_diff_pi, 0.75)],
    boots[, quantile(boot_diff_pf, 0.75)])
)

# make coeff plot from effects
differences[, position := 0]
differences[test == "Party Free Difference", position := 1]


pdf(file="plots/senate_difference_estimates.pdf", ## RENAME
  width = 5, height = 4, family = "Times")

plot(0, 0, type='n', ylim=c(-2.2, .3), xlim=c(-0.5, 1.5),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Effect")
axis(1, differences$position, cex.axis =.8,
  labels = c("Party Call Rate", "Baseline Rate"))
axis(2, c(-2, -1.5, -1, -0.5, 0), cex.axis = 1.1, labels = TRUE)
abline(h=0, col="gray55", xpd=FALSE)
title(main="Same-State Pair Differences, Reelection Treatment",
  cex.main=.8, line=0.75, font.main=2)
points(differences$position, differences$Estimate,
  pch=19, col="black", cex=.8)
segments(differences$position, differences$Lower_Bound,
  differences$position,  differences$Upper_Bound, lwd = 1)
segments(differences$position,
  differences$Lower_50, differences$position, differences$Upper_50, lwd=2)

dev.off()



# # Appendix


# Make Table 5
regs1 <- list()
for (i in 93:112) {
  cat("*** working on congress", i, "\n")
  regs1[[paste0("hou", i)]] <- check_signs(house_party_calls[[paste0("hou", i)]])
}
regs1 <- rbindlist(regs)
coef_signs <- table(regs$party_coef, regs$ideal_coef) / length(regs$vt)
xtable(coef_signs)


# Make Table 6
regs <- list()
for (i in 93:112) {
  cat("*** working on congress", i, "\n")
  regs[[paste0("sen", i)]] <- check_signs(senate_party_calls[[paste0("sen", i)]])
}
regs <- rbindlist(regs)
coef_signs <- table(regs$party_coef, regs$ideal_coef) / length(regs$vt)
xtable(coef_signs)


# Make Tables 7 & 8
naive_difference1[, placement := NULL]
seat_type_effect <- DATA[mean_tr == .5, sum(tr * y1) - sum((1 - tr) * y1),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_effect, "V1", "effect")
seat_type_placebo <- DATA[, sum((rand > mean(rand)) * y1) -
    sum(((rand) <= mean(rand)) * y1),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_placebo, "V1", "placebo")
seat_type_difference <- data.table(
  Test = c("2 Maj Dems Effect", "2 Maj Dems Placebo",
    "2 Min Dems Effect", "2 Min Dems Placebo",
    "2 Maj Reps Effect", "2 Maj Reps Placebo",
    "2 Min Reps Effect", "2 Min Reps Placebo",
    "Split, Maj Dem, Dem Effect", "Split, Maj Dem, Dem Placebo",
    "Split, Maj Dem, Rep Effect", "Split, Maj Dem, Rep Placebo",
    "Split, Maj Rep, Dem Effect", "Split, Maj Rep, Dem Placebo",
    "Split, Maj Rep, Rep Effect", "Split, Maj Rep, Rep Placebo"),
  DV = "pirate100",
  Estimate = c(
    seat_type_effect[seat_pair_type == "2 maj dems", effect],
    seat_type_placebo[seat_pair_type == "2 maj dems", placebo],
    seat_type_effect[seat_pair_type == "2 min dems", effect],
    seat_type_placebo[seat_pair_type == "2 min dems", placebo],
    seat_type_effect[seat_pair_type == "2 maj reps", effect],
    seat_type_placebo[seat_pair_type == "2 maj reps", placebo],
    seat_type_effect[seat_pair_type == "2 min reps", effect],
    seat_type_placebo[seat_pair_type == "2 min reps", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem, dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem, dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem, rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem, rep", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep, dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep, dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep, rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep, rep", placebo]
  )
)
naive_difference1_tex <- xtable(naive_difference1, auto = TRUE,
  caption = "Reelection and Response to Party Calls, Difference in Differences",
  digits = c(3, 3, 3, 3, 3, 3))
print(naive_difference1_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")
seat_type_difference_tex <- xtable(seat_type_difference, auto = TRUE,
  caption = "Diff in Diff, Subgroup Condition, Party Influenced Rate")
print(seat_type_difference_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")


# Make Tables 9 & 10
DATA[, stabb_congress := paste(stabb, congress)]
summary(lfe::felm(y ~ tr | stabb_congress | 0 | stabb_congress,
  DATA[mean_tr == .5]))
summary(lfe::felm(y ~ tr | stabb_congress | 0 | stabb_congress,
  DATA[mean_tr == 0]))

summary(lfe::felm(y ~ tr + factor(seat_pair_type) |
    stabb_congress | 0 | stabb_congress,
  DATA))


DATA_1 <- DATA[tr == 1]
DATA_0 <- DATA[tr == 0]
DATA_pairs <- merge(DATA_1, DATA_0, by = c("stabb", "congress", "seat_pair_type"))


seat_type_effect <- DATA[mean_tr == .5, sum(tr * y) - sum((1 - tr) * y),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_effect, "V1", "effect")
seat_type_effect

seat_type_placebo <- DATA[, sum((rand > mean(rand)) * y) - sum(((rand) <= mean(rand)) * y),
  .(stabb, congress, seat_pair_type)][, mean(V1), .(seat_pair_type)]
setnames(seat_type_placebo, "V1", "placebo")
seat_type_placebo

seat_type_difference <- data.table(
  Test = c("2 Maj Dems Effect", "2 Maj Dems Placebo",
    "2 Min Dems Effect", "2 Min Dems Placebo",
    "2 Maj Reps Effect", "2 Maj Reps Placebo",
    "2 Min Reps Effect", "2 Min Reps Placebo",
    "Split, Maj Dem, Dem Effect", "Split, Maj Dem, Dem Placebo",
    "Split, Maj Dem, Rep Effect", "Split, Maj Dem, Rep Placebo",
    "Split, Maj Rep, Dem Effect", "Split, Maj Rep, Dem Placebo",
    "Split, Maj Rep, Rep Effect", "Split, Maj Rep, Rep Placebo"),
  DV = "pirate100 - pfrate100",
  Estimate = c(
    seat_type_effect[seat_pair_type == "2 maj dems", effect],
    seat_type_placebo[seat_pair_type == "2 maj dems", placebo],
    seat_type_effect[seat_pair_type == "2 min dems", effect],
    seat_type_placebo[seat_pair_type == "2 min dems", placebo],
    seat_type_effect[seat_pair_type == "2 maj reps", effect],
    seat_type_placebo[seat_pair_type == "2 maj reps", placebo],
    seat_type_effect[seat_pair_type == "2 min reps", effect],
    seat_type_placebo[seat_pair_type == "2 min reps", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem, dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem, dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj dem, rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj dem, rep", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep, dem", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep, dem", placebo],
    seat_type_effect[seat_pair_type == "split/maj rep, rep", effect],
    seat_type_placebo[seat_pair_type == "split/maj rep, rep", placebo]
  )
)

naive_difference_tex <- xtable(naive_difference, auto = TRUE,
  caption = "Reelection and Response to Party Calls, Difference in Differences",
  digits = c(3, 3, 3, 3, 3, 3))
print(naive_difference_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")

seat_type_difference_tex <- xtable(seat_type_difference, auto = TRUE,
  caption = "Diff in Diff, Subgroup Condition, Party Influenced Rate")
print(seat_type_difference_tex, include.rownames = FALSE,
  table.placement = "H", caption.placement = "top")


# Make Table 11
mod1 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
    pres_vote_share + freshman + retiree  + best_committee + up_for_reelection +
    power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Democrat"])
mod2 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
    pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
    power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[caucus == "Republican"])
mod3 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
    pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
    power_committee + leader + chair | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[maj == 1])
mod4 <- felm(pirate100 ~ ideological_extremism + pfrate100 + vote_share +
    pres_vote_share + freshman + retiree + best_committee + up_for_reelection +
    power_committee + leader | icpsrLegis + congress | 0 | icpsrLegis,
  senate_data[maj == 0])
texreg::texreg(list(mod1, mod2, mod3, mod4),
  reorder.coef = c(1:2, 8, 3:7, 9:11))


# Make Table 12
texreg::texreg(list(
  lm(hou_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(hou_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(hou_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(hou_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(hou_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(hou_extremism, new_whoheeds13[dem == 0 & congress == 107])
), reorder.coef = c(2:15, 1))


# Make Figure 6
fm_extremism_hou <- function(i, j) {
  summary(lm(hou_extremism,
    data = subset(new_whoheeds13, congress == i & majority == j)),
    vcov = vcovHC(type = "HC1"))
}
B_hou <- SE_hou <- data.frame(row.names = 93:112)
B_hou$extremism_maj_hou <- sapply(93:112, function(x)
  fm_extremism_hou(x, 1)$coef["ideological_extremism", "Estimate"])
B_hou$extremism_min_hou <- sapply(93:112, function(x)
  fm_extremism_hou(x, 0)$coef["ideological_extremism", "Estimate"])
SE_hou$extremism_maj_hou <- sapply(93:112, function(x)
  fm_extremism_hou(x, 1)$coef["ideological_extremism", "Std. Error"])
SE_hou$extremism_min_hou <- sapply(93:112, function(x)
  fm_extremism_hou(x, 0)$coef["ideological_extremism", "Std. Error"])

pdf(file="plots/who-heeds-figure2-replication_lm.pdf",
  width = 4, height = 8, family = "Times")
layout(matrix(1:2, 2, 1, byrow = TRUE))
par(mar = c(2.5, 4, 2, 0.3) + 0.1, font.lab = 2)

x_hou <- (93:112)
x.ticks_hou <- c(94, 99, 104, 109)
y.ticks_hou <- c(-12, 0, 12)

b_hou <- B$extremism_maj_hou
se_hou <- SE$extremism_maj_hou
plot(0, 0, type='n', ylim=c(-4, 20), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Majority")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Ideological Extremism", cex.main=1.15, line=0.75, font.main=2)
points(x_hou, b_hou, pch=19, col="black", cex=.8)
segments(x_hou, b_hou - qnorm(.750) * se, x_hou, b+qnorm(.750)*se, lwd=2)
segments(x_hou, b_hou - qnorm(.975) * se, x_hou, b+qnorm(.975)*se, lwd=.9)

b_hou <- B$extremism_min_hou
se_hou <- SE$extremism_min_hou
plot(0, 0, type='n', ylim=c(-4, 20), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Minority")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x_hou, b_hou, pch=19, col="black", cex=.8)
segments(x_hou, b_hou - qnorm(.750)*se, x_hou, b+qnorm(.750)*se, lwd=2)
segments(x_hou, b_hou - qnorm(.975)*se, x_hou, b+qnorm(.975)*se, lwd=.9)

dev.off()


# Make Figure 7
fm_extremism <- function(i, j) {
  summary(lm(sen_extremism,
    data = subset(senate_data, congress == i & maj == j)),
    vcov = vcovHC(type = "HC1"))
}

B <- SE <- data.frame(row.names = 93:112)
B$extremism_maj <- sapply(93:112, function(x)
  fm_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B$extremism_min <- sapply(93:112, function(x)
  fm_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE$extremism_maj <- sapply(93:112, function(x)
  fm_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE$extremism_min <- sapply(93:112, function(x)
  fm_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

pdf(file="plots/senate-figure2-lm.pdf",
  width = 4, height = 8, family = "Times")
layout(matrix(1:2, 2, 1, byrow = TRUE))
par(mar = c(2.5, 4, 2, 0.3) + 0.1, font.lab = 2)

x <- (93:112)
x.ticks <- c(95, 100, 105, 110)
y.ticks <- c(- 24, -12, -6, 0, 12)

b <- B$extremism_maj#[-15]
se <- SE$extremism_maj#[-15]
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Majority Party")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="Ideological Extremism", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b - qnorm(.750) * se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b - qnorm(.975) * se, x, b+qnorm(.975)*se, lwd=.9)

b <- B$extremism_min#[-15]
se <- SE$extremism_min#[-15]
plot(0, 0, type='n', ylim=c(-8, 24), xlim=c(93, 112),
  cex.lab=1.15, xaxt="n", yaxt="n", xlab="", ylab="Minority Party")
axis(1, x.ticks, cex.axis=1.1, labels=TRUE, xpd=TRUE)
axis(2, y.ticks, cex.axis=1.1, labels=TRUE)
abline(h=0, col="gray", xpd=FALSE)
title(main="", cex.main=1.15, line=0.75, font.main=2)
points(x, b, pch=19, col="black", cex=.8)
segments(x, b - qnorm(.750)*se, x, b+qnorm(.750)*se, lwd=2)
segments(x, b - qnorm(.975)*se, x, b+qnorm(.975)*se, lwd=.9)

dev.off()
