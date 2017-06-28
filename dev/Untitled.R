library(partycalls)
library(ggplot2)
library(xtable)

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]  # house data

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

texreg::texreg(lm(sen_extremism, senate_data))
