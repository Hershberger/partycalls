library(partycalls)
library(ggplot2)

load("test_data/senate_party_calls_lm.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

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

regs <- list()
for (i in 93:112) {
  cat("*** working on congress", i, "\n")
  regs[[paste0("sen", i)]] <- check_signs(senate_party_calls[[paste0("sen", i)]])
}
regs <- rbindlist(regs)

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

# get 2x2 table of percent of vote
coef_signs <- table(regs$party_coef, regs$ideal_coef) / length(regs$vt)
xtable::xtable(coef_signs)
