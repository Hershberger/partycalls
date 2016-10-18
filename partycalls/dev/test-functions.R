# load data
options(stringsAsFactors = FALSE)
library(partycalls)
library(pscl)
library(emIRT)
library(data.table)
load("inst/extdata/houKHfiles001-111.rdata")


# run current versions of functions
symdiff <- function(x, y)
{
  sort(setdiff(base::union(x, y), base::intersect(x, y)))
}

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

code_party_calls_1step <- function(rc, DT, noncalls)
{
  rc1 <- rc
  rc2 <- rc
  rc1$votes <- rc$votes[, noncalls]
  rc2$votes <- rc$votes[, -noncalls]
  rc1$m <- ncol(rc1$votes)
  rc2$m <- ncol(rc2$votes)
  p <- makePriors(rc1$n, rc1$m, 1)
  s <- getStarts(rc1$n, rc1$m, 1)
  sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
    "NUL"
  } else {
    "/dev/null"
  }
  sink(sink_target)
  l <- binIRT(.rc = rc1, .starts = s, .priors = p,
    .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
  sink()
  unlink(sink_target)
  DT$x <- l$means$x
  regs <- DT[party %in% c("D", "R"), test_rollcall(.SD), .(vt)]
  pvals <- regs$p
  pvals[is.na(pvals)] <- 1
  pvals
  ok <- pvals > .05
  which(ok)
}

code_party_calls <- function(rc, pval_threshold = 0.01, count_min = 15,
  count_max = 150, match_count_min = 10, sim_annealing = TRUE,
  random_seed = TRUE, lopside_thresh = 0.65, vote_switch_percent = 0.01)
{
  rc <- pscl::dropRollCall(rc, dropList = alist(dropLegis = state == "USA"))
  rc <- emIRT::convertRC(rc, type = "binIRT")
  DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  DT$y <- as.vector(rc$votes)
  DT$party <- rc$legis.data$party
  DT[y %in% c(0, 9), y:= NA]
  DT[y == -1, y:= 0]
  if (random_seed == TRUE) {
    noncalls <- sample(rc$m, floor(.5 * rc$m))
  } else {
    noncalls_DT <- DT[, yea_perc := mean(y, na.rm = TRUE), by = vt]
    noncalls_DT <-
      subset(DT, yea_perc < lopside_thresh & yea_perc > 1 - lopside_thresh,
        select = vt)
    noncalls_DT <- c(unique(noncall_DT$vt))
    noncalls <-
      as.numeric(c(gsub(pattern = "Vote ", replacement = "", noncalls_DT)))
  }
  switched_votes <- seq_len(rc$m)
  match_counter <- 0
  counter <- 0
  record_of_coding <- list()
  record_of_pvals <- list()
  while (counter <= count_min |
      (counter < count_max & match_counter < match_count_min)) {
    counter <- counter + 1
    record_of_coding[[counter]] <- noncalls
    old_noncalls <- noncalls
    old_switched_votes <- switched_votes
    pvals <- code_party_calls_1step(rc, DT, noncalls)
    record_of_pvals[[counter]] <- pvals
    noncalls <- which(pvals > pval_threshold)
    calls <- setdiff(seq_len(rc$m), old_noncalls)
    if (sim_annealing == TRUE) {
      n_random_switches <- floor(rc$m * .2 * max(0, 1 - counter / 50) ^ 2)
    } else {
      n_random_switches <- 0
    }
    if (n_random_switches > 0) {
      calls_to_switch <- sample(calls, n_random_switches)
      noncalls_to_switch <- sample(noncalls, n_random_switches)
      calls_to_keep <- setdiff(calls, calls_to_switch)
      noncalls_to_keep <- setdiff(noncalls, noncalls_to_switch)
      calls <- c(calls_to_keep, noncalls_to_switch)
      noncalls <- c(noncalls_to_keep, calls_to_switch)
    }
    switched_votes <- symdiff(noncalls, old_noncalls)
    if (length(switched_votes) <= vote_switch_percent * rc$m) {
      match_counter <- match_counter + 1
    } else {
      match_counter <- 0
    }
    cat("Iteration", counter, "had", length(switched_votes), "out of", rc$m,
      "switched votes\n")
  }
  rc$party_calls <- seq_len(rc$m)[-noncalls]
  rc$record_of_coding <- record_of_coding
  rc$record_of_pvals <- record_of_pvals
  rc
}

# test functions with data
code_party_calls(rc = h093)
