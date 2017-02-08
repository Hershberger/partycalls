
#' Run the party calls classifier
#'
#' Use the iterative algorithm that (1) predicts ideal points based on last
#' iteration's non-party calls, (2) runs regressions of roll call vote on
#' ideal points and party, (3) classify new iteration of non-party calls as
#' votes for which the p value on party is > .05.
#' @param rc a rollcall object
#' @return rollcall object with record of classification algorithm and
#' list of classified party calls
#' @import data.table emIRT pscl
#' @export
code_party_calls <- function(rc)
{
  rc <- pscl::dropRollCall(rc, dropList = alist(dropLegis = state == "USA"))
  ## BEGIN NEW
  rc <- pscl::dropRollCall(rc, dropList = alist(lop = 4))
  ## END NEW
  rc <- emIRT::convertRC(rc, type = "binIRT")
  DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  DT$y <- as.vector(rc$votes)
  DT$party <- rc$legis.data$party
  DT[y %in% c(0, 9), y:= NA]
  DT[y == -1, y:= 0]
  ## BEGIN REPLACE
  #noncalls <- sample(rc$m, floor(.5 * rc$m))
  ## REPLACED WITH
  noncalls_DT <- DT[, .(yea_perc = mean(y, na.rm = TRUE)), by = vt]
  noncalls <- which(noncalls_DT[, yea_perc <= .35 | .65 <= yea_perc])
  ## END REPLACE
  ## BEGIN NEW
  match_switch <- FALSE
  ## END NEW
  switched_votes <- seq_len(rc$m)
  match_counter <- 0
  counter <- 0
  record_of_coding <- list()
  while (counter <= 15 |
      ## BEGIN REPLACE
      # (counter < 100 & match_counter < 10 & length(switched_votes) > 0)) {
      ## REPLACED WITH
      (counter < 100 & match_counter < 15 & length(switched_votes) > 0)) {
      ## END REPLACE
    record_of_coding[[counter + 1]] <- noncalls
    old_noncalls <- noncalls
    old_switched_votes <- switched_votes
    noncalls <- code_party_calls_1step(rc, DT, noncalls)
    switched_votes <- symdiff(noncalls, old_noncalls)
    ## BEGIN REPLACE
    # if (length(switched_votes) <= 5) {
    #   match_counter <- match_counter + 1
    # } else {
    #   match_counter <- 0
    # }
    # counter <- counter + 1
    # cat("Iteration", counter, "had", length(switched_votes), "out of", rc$m,
    #   "switched votes\n")
    ## REPLACED WITH
    counter <- counter + 1
    countdown <- ""
    if ((length(switched_votes) > length(old_switched_votes) |
        length(switched_votes) <= 5) &
        counter > 15) {
      match_switch <- TRUE
    }
    if (match_switch) {
      match_counter <- match_counter + 1
      countdown <- paste0("(", 15 -  match_counter, " iterations left)")
    }
    cat("Iteration", counter, "had", length(switched_votes), "out of", rc$m,
      "switched votes", countdown, "\n")
    ## END REPLACE
  }
  rc$party_calls <- seq_len(rc$m)[-noncalls]
  rc$record_of_coding <- record_of_coding
  rc
}


#' Run one step of the party calls algorithm
#'
#' To be used inside a call to code_party_calls
#' @param rc a rollcall object
#' @param DT data.table with votes and party indicators
#' @param noncalls indices for non-party calls from last run
#' @return vector of indices for non-party calls
code_party_calls_1step <- function(rc, DT, noncalls)
{
  rc1 <- rc
  rc2 <- rc
  rc1$votes <- rc$votes[, noncalls]
  rc2$votes <- rc$votes[, -noncalls]
  rc1$m <- ncol(rc1$votes)
  rc2$m <- ncol(rc2$votes)
  p <- emIRT::makePriors(rc1$n, rc1$m, 1)
  s <- emIRT::getStarts(rc1$n, rc1$m, 1)
  sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
    "NUL"
  } else {
    "/dev/null"
  }
  sink(sink_target)
  l <- emIRT::binIRT(.rc = rc1, .starts = s, .priors = p,
    .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
  sink()
  unlink(sink_target)
  DT$x <- l$means$x
  regs <- DT[party %in% c("D", "R"), test_rollcall(.SD), .(vt)]
  pvals <- regs$p
  pvals[is.na(pvals)] <- 1
  ok <- pvals > .05
  which(ok)
}

#' Find symmetric difference
#'
#' The symmetric difference is defined as the set difference between the
#' union of two sets and the intersection of those sets.
#' @param x vector
#' @param y vector
#' @return vector of elements in either x or y but not both
symdiff <- function(x, y)
{
  sort(setdiff(base::union(x, y), base::intersect(x, y)))
}


#' Regress a single roll call on party indicator and ideal points
#'
#' To be used inside a call to code_party_calls_1step
#' @param .SD subset of a data.table of roll call votes, with a column for party
#' labels
#' @return list of coefficient, standard error, t value, and p value for the
#' coefficient on party
test_rollcall <- function(.SD)
{
  ## BEGIN NEW
  .SD <- .SD[party %in% c("D", "R")]
  n_yea_reps <- .SD[, sum(y == 1 & party == "R", na.rm = TRUE)]
  n_nay_reps <- .SD[, sum(y == 0 & party == "R", na.rm = TRUE)]
  n_yea_dems <- .SD[, sum(y == 1 & party == "D", na.rm = TRUE)]
  n_nay_dems <- .SD[, sum(y == 0 & party == "D", na.rm = TRUE)]
  party_line_vote <-
    (n_yea_reps == 0 & n_nay_reps >  0 & n_yea_dems >  0 & n_nay_dems == 0) |
    (n_yea_reps >  0 & n_nay_reps == 0 & n_yea_dems == 0 & n_nay_dems >  0)
  ## END NEW
  if (mean(.SD[, y], na.rm = TRUE) %in% c(0:1, NaN) |
      length(unique(.SD[!is.na(y) & party %in% c("D", "R"), party])) == 1L) {
    list(b = 0, se = 0, t = Inf, p = NA_real_)
  ## BEGIN NEW
  } else if (party_line_vote) {
      list(b = 1, se = 0, t = Inf, p = 0)
  ## END NEW
  } else {
    ## BEGIN REPLACE
    # m <- lm(y ~ party + x, data = .SD)
    # suppressWarnings(summ <- summary(m)$coef["partyR", ])
    # list(b = summ["Estimate"], se = summ["Std. Error"],
    #   t = summ["t value"], p = summ["Pr(>|t|)"])
    ## REPLACED WITH
    m <- brglm::brglm(y ~ party + x, data = .SD, family = binomial)
    suppressWarnings(summ <- summary(m)$coef["partyR", ])
    list(b = summ["Estimate"], se = summ["Std. Error"],
      t = summ["z value"], p = summ["Pr(>|z|)"])
    ## END REPLACE
  }
}
