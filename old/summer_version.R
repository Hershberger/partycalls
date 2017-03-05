
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
  rc <- emIRT::convertRC(rc, type = "binIRT")
  DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  DT$y <- as.vector(rc$votes)
  DT$party <- rc$legis.data$party
  DT[y %in% c(0, 9), y:= NA]
  DT[y == -1, y:= 0]
  noncalls <- sample(rc$m, floor(.5 * rc$m))
  switched_votes <- seq_len(rc$m)
  match_counter <- 0
  counter <- 0
  record_of_coding <- list()
  while (counter <= 15 |
      (counter < 100 & match_counter < 10 & length(switched_votes) > 0)) {
    record_of_coding[[counter + 1]] <- noncalls
    old_noncalls <- noncalls
    old_switched_votes <- switched_votes
    noncalls <- code_party_calls_1step(rc, DT, noncalls)
    switched_votes <- symdiff(noncalls, old_noncalls)
    #m <- 1 - length(switched_votes) / rc$m
    if (length(switched_votes) <= 5) {
      match_counter <- match_counter + 1
    } else {
      match_counter <- 0
    }
    counter <- counter + 1
    cat("Iteration", counter, "had", length(switched_votes), "out of", rc$m,
      "switched votes\n")
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
  if (mean(.SD[, y], na.rm = TRUE) %in% c(0:1, NaN) |
      length(unique(.SD[!is.na(y) & party %in% c("D", "R"), party])) == 1L) {
    list(b = 0, se = 0, t = Inf, p = NA_real_)
  } else {
    m <- lm(y ~ party + x, data = .SD)
    suppressWarnings(summ <- summary(m)$coef["partyR", ])
    list(b = summ["Estimate"], se = summ["Std. Error"],
      t = summ["t value"], p = summ["Pr(>|t|)"])
  }
}
