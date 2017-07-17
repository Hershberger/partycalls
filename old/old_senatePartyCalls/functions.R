#' Calculate congress corresponding to a year
#'
#' Simple conversion between year and congress
#' @param year integer year
#' @return integer congress
#' @export
calc_congress <- function(year)
{
  (year - 1788) / 2
}

#' Calculate year corresponding to a congress
#'
#' Simple conversion between congress and year
#' @param congress integer congress
#' @return integer year
#' @export
calc_year <- function(congress)
{
  1788 + 2 * congress
}

#' Calculate election year corresponding to a congress
#'
#' Calculate year of most previous regular election based on congress and class
#' class 1: up for reelection in 1964, 1970, 1976, 1982, 1988, 1994, 2000, 2006,
#'  2012; congresses in seq(94, 112, 3)
#' class 2: up for reelection in 1966, 1972, 1978, 1984, 1990, 1996, 2002, 2008,
#'  2014; congresses in seq(95, 113, 3)
#' class 3: up for reelection in 1968, 1974, 1980, 1986, 1992, 1998, 2004, 2010,
#'  2016; congresses in seq(93, 114, 3)
#' @param congress integer congress
#' @param class integer 1, 2, or 3; see Detail
#' @return integer year
#' @export
calc_year_elected <- function(congress, class)
{
  offset <- c(1, 0, 2)[class]
  1786 + 2 * (congress - ((congress + offset) %% 3))
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
  ok <- pvals > .05
  which(ok)
}

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

get_gray_votes <- function(record_of_coding)
{
  record_of_coding <- tail(record_of_coding, 10)
  all_votes_ever_classied_as_calls <- Reduce(union, record_of_coding)
  all_votes_always_classied_as_calls <- Reduce(intersect, record_of_coding)
  paste("Vote", setdiff(all_votes_ever_classied_as_calls,
    all_votes_always_classied_as_calls))
}

#' Make dataset at level of senator/year
#'
#' For a single congress, assemble data for analysis
#' @param congress congress ID number
#' @param roll_calls_object_list rc a list of rollcall objects with classified
#' party calls
#' @return data.table with party-free ideal points
#' @import data.table emIRT pscl
#' @export
make_senator_year_data <- function(congress, roll_calls_object_list)
{
  rc <- roll_calls_object_list[[paste0("sen", congress)]]
  ld <- rc$legis.data
  ld$mc <- rownames(ld)
  setDT(ld)
  votes <- rc$votes
  votes <- melt(votes)
  setDT(votes)
  setnames(votes, c("mc", "vote_id", "vote"))
  votes <- merge(votes, ld, by = "mc")
  gray_vote_ids <- get_gray_votes(rc$record_of_coding)
  party_call_vote_ids <- setdiff(paste("Vote", rc$party_calls), gray_vote_ids)
  noncall_vote_ids <- setdiff(paste("Vote", setdiff(1:rc$m, rc$party_calls)),
    gray_vote_ids)
  votes[, gray := as.numeric(vote_id %in% gray_vote_ids)]
  votes[, party_yea_rate := sum(vote == 1) / sum(vote %in% c(1, -1)),
    .(vote_id, party)]
  votes[, party_pos :=
      as.numeric(party_yea_rate > .5) - as.numeric(party_yea_rate < .5)]
  votes[, party_call := as.numeric(vote_id %in% party_call_vote_ids)]
  votes[, noncall := as.numeric(vote_id %in% noncall_vote_ids)]
  senator_year_data <- votes[vote %in% c(1, -1) & party_pos != 0,
    .(
      responsiveness_party_calls =
        mean(vote[party_call == 1] == party_pos[party_call == 1]),
      responsiveness_noncalls =
        mean(vote[noncall == 1] == party_pos[noncall == 1]),
      n_party_calls = sum(party_call == 1),
      n_noncalls = sum(noncall == 1)
      ), mc]
  senator_year_data <- merge(senator_year_data, ld, by = "mc")
  rc_noncalls <- rc
  rc_noncalls$votes <- rc_noncalls$votes[, noncall_vote_ids]
  rc_noncalls$m <- ncol(rc_noncalls$votes)
  p <- makePriors(rc_noncalls$n, rc_noncalls$m, 1)
  s <- getStarts(rc_noncalls$n, rc_noncalls$m, 1)
  sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
    "NUL"
  } else {
    "/dev/null"
  }
  sink(sink_target)
  fitted_emIRT <- binIRT(.rc = rc_noncalls, .starts = s, .priors = p,
    .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
  sink()
  unlink(sink_target)
  ideal <- as.data.frame(fitted_emIRT$means$x)
  ideal$mc <- rownames(ideal)
  setDT(ideal)
  setnames(ideal, c("pf_ideal", "mc"))
  senator_year_data <- merge(senator_year_data, ideal, by = "mc")
  senator_year_data$congress <- congress
  is_orientation_correct <- senator_year_data[party == "R", mean(pf_ideal)] >
    senator_year_data[party == "D", mean(pf_ideal)]
  if (!is_orientation_correct) {
    senator_year_data[, pf_ideal := -pf_ideal]
  }
  senator_year_data[, pf_ideal := pf_ideal / sd(pf_ideal)]
  senator_year_data[, pf_ideal := 5 + pf_ideal - mean(pf_ideal)]
  senator_year_data[, dist_from_floor_median := abs(pf_ideal - median(pf_ideal))]
  senator_year_data[party == "D", dist_from_party_median := abs(pf_ideal - median(pf_ideal))]
  senator_year_data[party == "R", dist_from_party_median := abs(pf_ideal - median(pf_ideal))]
  senator_year_data[, ideological_extremism := pf_ideal]
  senator_year_data[party == "D", ideological_extremism := -pf_ideal]
  list(senator_year_data = senator_year_data, fitted_emIRT = fitted_emIRT)
}
