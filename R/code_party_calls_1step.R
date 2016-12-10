#' Run one step of the party calls algorithm
#'
#' To be used inside a call to code_party_calls
#' @param rc a rollcall object
#' @param DT data.table with votes and party indicators
#' @param noncalls indices for non-party calls from last run
#' @param return_pvals logical for whether to return pvals or tvals
#' @param type character string, one of brglm, glm, or lm; which function
#' to use for roll call-by-roll call regression
#' @return vector of indices for non-party calls
#' @export
#' @importFrom emIRT binIRT makePriors getStarts
code_party_calls_1step <- function(rc, DT, noncalls, return_pvals,
  type = c("brglm", "lm", "glm"))
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
  regs <- DT[party %in% c("D", "R"), test_rollcall(.SD, type), .(vt)]
  if (return_pvals) {
    pvals <- regs$p
    pvals[is.na(pvals)] <- 1
    out <- pvals
  } else {
    tvals <- regs$t
    tvals[is.na(tvals)] <- Inf
    out <- tvals
  }
  out
}
