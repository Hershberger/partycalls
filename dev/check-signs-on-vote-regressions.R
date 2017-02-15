library(partycalls)
library(ggplot2)

load("test_data/senate_party_calls_p_05.RData")

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
  p <- emIRT::makePriors(rc$n, rc$m, 1)
  s <- emIRT::getStarts(rc$n, rc$m, 1)
  sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
    "NUL"
  } else {
    "/dev/null"
  }
  sink(sink_target)
  l <- emIRT::binIRT(.rc = rc, .starts = s, .priors = p,
    .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
  sink()
  unlink(sink_target)

  ideal_dt <- merge(
    data.table(x = l$means$x[, 1], mc = rownames(l$means$x)),
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


  regs <- DT[party %in% c("D", "R"), test_rollcall2(.SD, type = "brglm"), vt]
  levels <- c("negative significant", "negative insignificant",
    "positive insignificant", "positive significant")
  regs[, party_coef := "positive significant"]
  regs[party_t > 0 & party_t <= qnorm(.975), party_coef := "positive insignificant"]
  regs[party_t > qnorm(.025) & party_t <= 0, party_coef := "negative insignificant"]
  regs[party_t <= qnorm(.025), party_coef := "negative significant"]
  regs[, ideal_coef := "positive significant"]
  regs[ideal_t > 0 & ideal_t <= qnorm(.975), ideal_coef := "positive insignificant"]
  regs[ideal_t > qnorm(.025) & ideal_t <= 0, ideal_coef := "negative insignificant"]
  regs[ideal_t <= qnorm(.025), ideal_coef := "negative significant"]
  regs[, party_coef := factor(party_coef, levels = levels)]
  regs[, ideal_coef := factor(ideal_coef, levels = levels)]
  regs[, party_vote := "non-party vote"]
  regs[n_yea_dems > n_nay_dems & n_yea_reps < n_nay_reps, party_vote := "party vote"]
  regs[n_yea_dems < n_nay_dems & n_yea_reps > n_nay_reps, party_vote := "party vote"]
  regs[, n_yeas :=  n_yea_dems + n_yea_reps]
  regs[, n_nays :=  n_nay_dems + n_nay_reps]
  regs[, pct_yeas := n_yeas / (n_yeas + n_nays)]
  regs[, pct_nays := 1 - pct_yeas]
  regs[, close_vote := "lopsided"]
  regs[pct_yeas > .35 & pct_yeas < .65, close_vote := "close"]
  regs[, party_call := "noncall"]
  regs[vt %in% colnames(rc$votes)[rc$party_calls], party_call := "call"]
  regs
}

rc <- partycalls::code_party_calls_by_congress_number(93,
  chamber = "senate", pval_threshold = 0.05,  type = "brglm")

regs <- check_signs(rc)

# regs <- rbindlist(lapply(senate_party_calls, check_signs))

ggplot(regs,
  aes(party_t, ideal_t, color = party_call)) +
  facet_grid(party_vote ~ close_vote) +
  theme_minimal() +
  geom_point(alpha = .5) +
  coord_equal() +
  geom_hline(yintercept = qnorm(.975), linetype = 3, color = "black") +
  geom_hline(yintercept = qnorm(.025), linetype = 3, color = "black") +
  geom_vline(xintercept = qnorm(.975), linetype = 3, color = "black") +
  geom_vline(xintercept = qnorm(.025), linetype = 3, color = "black")#+
  #geom_smooth(se = FALSE, method = "lm")
