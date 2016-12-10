#' TITLE
#'
#' DETAIL
#' @param rc xxx
#' @param DT xxx
#' @param tvals xxx
#' @param type xxx
#' @export
#' @return xxx
test_classification <- function(rc, DT, noncalls, tvals, type)
{
  rc_noncalls <- rc
  rc_noncalls$votes <- rc$votes[, noncalls]
  rc_noncalls$m <- ncol(rc_noncalls$votes)
  p <- emIRT::makePriors(rc_noncalls$n, rc_noncalls$m, 1)
  s <- emIRT::getStarts(rc_noncalls$n, rc_noncalls$m, 1)
  sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
    "NUL"
  } else {
    "/dev/null"
  }
  sink(sink_target)
  l <- emIRT::binIRT(.rc = rc_noncalls, .starts = s, .priors = p,
    .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
  sink()
  unlink(sink_target)
  DT <- merge(DT, data.table(mc = rownames(l$means$x), x = l$means$x[, "d1"]),
    by = "mc")
  regs <- DT[party %in% c("D", "R"), test_rollcall(.SD, type), .(vt)]
  classification_distance <- sum(abs(tvals - regs$t), na.rm = TRUE)
  list(regs = regs, classification_distance = classification_distance)
}
