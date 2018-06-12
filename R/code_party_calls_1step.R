
#' Run one step of the party calls algorithm
#'
#' To be used inside a call to code_party_calls
#' @param rc a rollcall object
#' @param DT data.table with votes and party indicators
#' @param votes_for_ideal_point_estimation indices for votes to use in ideal
#' point estimation
#' @param return_pvals logical for whether to return pvals or tvals
#' @param type character string, one of brglm, glm, or lm; which function
#' to use for roll call-by-roll call regression
#' @return list of pvals or tvals and ideal points
#' @export
#' @importFrom emIRT binIRT makePriors getStarts
code_party_calls_1step <- function(rc, DT, votes_for_ideal_point_estimation,
  return_pvals, type = c("brglm", "lm", "glm", "lasso"))
{
  rc1 <- rc
  rc2 <- rc
  rc1$votes <- rc$votes[, votes_for_ideal_point_estimation]
  rc2$votes <- rc$votes[, -votes_for_ideal_point_estimation]
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
  ideal <- l$means$x
  party <- rc1$legis.data$party
  orientation_correct <- mean(ideal[party == "D"]) < mean(ideal[party == "R"])
  if (!orientation_correct) {
    ideal <- -ideal
  }
  regs <- DT[party %in% c("D", "R"), test_rollcall(.SD, type), .(vt)]
  if (type != "lasso") {
    pvals <- regs$p
    pvals[is.na(pvals)] <- 1
    tvals <- regs$t
    tvals[is.na(tvals)] <- Inf
    list(pvals = pvals, tvals = tvals, ideal = ideal)
  } else {
    list(coefs = regs$V1, ideal = ideal)
  }
}
