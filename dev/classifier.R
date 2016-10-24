library(partycalls)
library(emIRT)
library(data.table)
library(ggplot2)
options(stringsAsFactors = FALSE)
load("inst/extdata/houKHfiles001-111.rdata")
congress_number <- 102
rc <- get(paste0("h", sprintf("%03.f", congress_number)))
rc <- pscl::dropRollCall(rc, dropList = alist(dropLegis = state == "USA"))
rc <- pscl::dropRollCall(rc, dropList = alist(lop = 4))
rc <- emIRT::convertRC(rc, type = "binIRT")
DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
DT$y <- as.vector(rc$votes)
DT$party <- rc$legis.data$party
DT[y %in% c(0, 9), y:= NA]
DT[y == -1, y:= 0]
n_votes <- length(unique(DT$vt))

classifier <- function(classification)
{
  noncalls <- which(classification == 1)
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
  regs <- DT[party %in% c("D", "R"), test_rollcall(.SD, type = "brglm"), .(vt)]
  new_classification <- 1 * (abs(regs$t) < 2.32)
  sum(classification != new_classification)
}

classifier(rbinom(n_votes, 1, .5))

library(parallel)
cl <- makeCluster(4)
clusterExport(cl, c("classifier", "rc", "DT"))
clusterEvalQ(cl, library(partycalls))
library(rgenoud)
optimizer <- genoud(classifier, n_votes, pop.size = 16, max.generations = 10,
  wait.generations = 0,
  Domains = matrix(c(rep(0, n_votes), rep(1, n_votes)), n_votes),
  data.type.int = TRUE, cluster = cl)
optimizer$value
