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
#' @param use_brglm logical for whether to use the bias-reduced glm logit
#' @return list of coefficient, standard error, t value, and p value for the
#' coefficient on party
test_rollcall <- function(.SD, use_brglm = TRUE, spiders = FALSE) #spiders?
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
    list(b = 0, se = 0, t = Inf, p = NA_real_)
  } else if (party_line_vote) {
    list(b = 1, se = 0, t = Inf, p = 0)
  } else {
    if (use_brglm) {
      m <- brglm::brglm(y ~ party + x, data = .SD, family = binomial)
    } else {
      m <- glm(y ~ party + x, data = .SD, family = binomial)
    }
    suppressWarnings(summ <- summary(m)$coef["partyR", ])
    list(b = summ["Estimate"], se = summ["Std. Error"],
      t = summ["z value"], p = summ["Pr(>|z|)"])
  }
}

#' Run one step of the party calls algorithm
#'
#' To be used inside a call to code_party_calls
#' @param rc a rollcall object
#' @param DT data.table with votes and party indicators
#' @param noncalls indices for non-party calls from last run
#' @param return_pvals logical for whether to return pvals or tvals
#' @return vector of indices for non-party calls
code_party_calls_1step <- function(rc, DT, noncalls, return_pvals)
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
  if (return_pvals) {
    pvals <- regs$p
    pvals[is.na(pvals)] <- 1 # is this already taking care of party line votes?
    out <- pvals
  } else {
    tvals <- regs$t
    tvals[is.na(tvals)] <- Inf # is this already taking care of party line votes?
    out <- tvals
  }
  out
}

#' Run the party calls classifier
#'
#' Use the iterative algorithm that (1) predicts ideal points based on last
#' iteration's non-party calls, (2) runs regressions of roll call vote on
#' ideal points and party, (3) classify new iteration of non-party calls as
#' votes for which the p value on party is greater than a user specified value,
#' preset at 0.01.
#' @param rc a rollcall object
#' @param pval_threshold the p-value required to code a vote as a party call.
#' Its default setting is 0.01
#' @param tval_threshold the t-value required to code a vote as a party call.
#' @param count_min The minimum count of iterations for the algorithm to run
#' before returning a result. The default setting is 15.
#' @param count_max The maximum count of iterations for the algorithm to run
#' before returning a result. The default setting is 150.
#' @param match_count_min The minimum number of iterations which fall below the
#' acceptable switched vote threshold. The default setting is 10.
#' @param sim_annealing If set to TRUE, runs a simulated annealing process to
#' avoid the algorithm from staying at local maxima. The default value is TRUE.
#' @param random_seed If set to TRUE, randomly draws votes from the rc object to
#' use as the initial classification for party calls. If set to false, the
#' initial classification of party calls will be lopsided votes as defined by
#' the user. The default value is TRUE.
#' @param lopside_thresh The threshold for classification of lopsided votes if
#' the option to randomly seed initial calls is set to FALSE. The default
#' setting is 0.65
#' @param vote_switch_percent The maximum percent of votes allowed to switch in
#' an iteration of the algorithm while contributing to the match counter. When
#' this threshold is exceeded the counter resets to 0. The default setting is
#' 0.01.
#' @param drop_very_lopsided_votes logical for whether to drop all votes
#' with fewer than 5 yeas or fewer than 5 nays
#' @param n_iterations_for_coding number of iterations from the end of the
#' process to use to code party calls, noncalls, and gray votes
#' @param use_new_match_check logical for whether to use new procedure
#' to check for matches between iterations
#' @return rollcall object with record of classification algorithm and
#' list of classified party calls
#' @import data.table emIRT pscl
#' @export
code_party_calls <- function(rc,
  pval_threshold = 0.01, tval_threshold = 2.32, count_min = 10,
  count_max = 150, match_count_min = 150, sim_annealing = TRUE,
  random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
  drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
  n_iterations_for_coding = 5, use_new_match_check = TRUE)
{
  rc <- pscl::dropRollCall(rc, dropList = alist(dropLegis = state == "USA"))
  if (drop_very_lopsided_votes) {
    rc <- pscl::dropRollCall(rc, dropList = alist(lop = 4))
  }
  rc <- emIRT::convertRC(rc, type = "binIRT")
  DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  DT$y <- as.vector(rc$votes)
  DT$party <- rc$legis.data$party
  DT[y %in% c(0, 9), y:= NA]
  DT[y == -1, y:= 0]

  if (random_seed == TRUE) {
    noncalls <- sample(rc$m, floor(.5 * rc$m))
  } else {
    stopifnot(lopside_thresh > 0 & lopside_thresh < 1 & lopside_thresh != .5)
    LB <- min(1 - lopside_thresh, lopside_thresh)
    UB <- max(1 - lopside_thresh, lopside_thresh)
    # noncalls_DT <- DT[, yea_perc := mean(y, na.rm = TRUE), by = vt]
    # you don't want to mix "<-" with ":=" (the point of the latter is to avoid
    # the former). so do this instead:
    noncalls_DT <- DT[, .(yea_perc = mean(y, na.rm = TRUE)), by = vt]

    # noncalls_DT <-
    #   subset(DT, yea_perc < lopside_thresh & yea_perc > 1 - lopside_thresh,
    #     select = vt)
    # you can use data.table do this; as here:
    # noncalls_DT <- DT[yea_perc < lopside_thresh & yea_perc > 1 - lopside_thresh,
    #   vt]
    #   (I think this will rule out all votes, since none have both
    #   yea_perc < lopside_thresh & yea_perc > 1 - lopside_thresh
    #   so you want a "|" instead of a "&"; am I right about that?)

    # noncalls_DT <- c(unique(noncall_DT$vt))
    # noncalls <-
    #   as.numeric(c(gsub(pattern = "Vote ", replacement = "", noncalls_DT)))
    # noncalls_DT <- c(unique(noncall_DT$vt))
    # noncalls <-
    #   as.numeric(c(gsub(pattern = "Vote ", replacement = "", noncalls_DT)))
    # but actually, we just want to know which votes are lopsided. so we should
    # use "which"
    noncalls <- which(noncalls_DT[, yea_perc <= LB | UB <= yea_perc])
  }
  switched_votes <- seq_len(rc$m)
  match_counter <- 0
  counter <- 0
  record_of_coding <- list()
  match_switch <- FALSE # for old match checking procedure

  if (return_pvals) {
    record_of_pvals <- list()
  } else {
    record_of_tvals <- list()
  }
  while (counter <= count_min |
      (counter < count_max & match_counter < match_count_min)) {
    counter <- counter + 1
    record_of_coding[[counter]] <- noncalls
    old_noncalls <- noncalls
    old_switched_votes <- switched_votes
    if (return_pvals) {
      pvals <- code_party_calls_1step(rc, DT, noncalls, return_pvals)
      record_of_pvals[[counter]] <- pvals
      noncalls <- which(pvals > pval_threshold)
    } else {
      tvals <- code_party_calls_1step(rc, DT, noncalls, return_pvals)
      record_of_tvals[[counter]] <- tvals
      noncalls <- which(abs(tvals) < tval_threshold)
    }
    calls <- setdiff(seq_len(rc$m), old_noncalls) # this codes all noncalls as calls
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
    if (use_new_match_check) {
      if (length(switched_votes) <= vote_switch_percent * rc$m) {
        match_counter <- match_counter + 1
      } else {
        match_counter <- 0
      }
    } else {
      if (length(switched_votes) > length(old_switched_votes) &
          counter > count_min) {
        match_switch <- TRUE
      }
    }
    if (match_switch) {
      match_counter <- match_counter + 1
    }
    cat("Iteration", counter, "had", length(switched_votes), "out of", rc$m,
      "switched votes\n")
  }
  rc$party_calls <- seq_len(rc$m)[-noncalls]
  rc$record_of_coding <- record_of_coding
  if (return_pvals) {
    rc$record_of_pvals <- record_of_pvals
  } else {
    rc$record_of_tvals <- record_of_tvals
  }
  rc$party_call_coding <- get_party_call_coding(rc, n_iterations_for_coding)
  rc
}


#' @export
get_party_call_coding <- function(rc, n_iterations)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_always_classied_as_noncalls <- Reduce(intersect, record_of_coding)
  all_votes_ever_classied_as_noncalls <- Reduce(union, record_of_coding)
  gray_votes <- setdiff(all_votes_ever_classied_as_noncalls,
    all_votes_always_classied_as_noncalls)
  partycalls <- setdiff(seq_len(rc$m), all_votes_ever_classied_as_noncalls)
  noncalls <- all_votes_always_classied_as_noncalls
  voteno <- colnames(rc$votes)
  coding <- rep(NA, length(voteno))
  coding[gray_votes] <- "gray"
  coding[partycalls] <- "party call"
  coding[noncalls] <- "noncall"
  data.table(voteno, coding)
}


#' @export
get_party_calls <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_always_classied_as_calls <- Reduce(intersect, record_of_coding)
  paste("Vote", all_votes_always_classied_as_calls)
}

#' @export
get_noncalls <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_ever_classied_as_calls <- Reduce(union, record_of_coding)
  noncalls <- setdiff(colnames(rc$votes), all_votes_ever_classied_as_calls)
  paste("Vote", noncalls)
}

#' @export
get_gray_votes <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_ever_classied_as_calls <- Reduce(union, record_of_coding)
  all_votes_always_classied_as_calls <- Reduce(intersect, record_of_coding)
  gray_votes <- setdiff(all_votes_ever_classied_as_calls,
    all_votes_always_classied_as_calls)
  paste("Vote", gray_votes)
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
make_member_year_data <- function(congress, roll_calls_object_list)
{
  rc <- roll_calls_object_list[[paste0("hou", congress)]]
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
  member_year_data <- votes[vote %in% c(1, -1) & party_pos != 0,
    .(
      responsiveness_party_calls =
        mean(vote[party_call == 1] == party_pos[party_call == 1]),
      responsiveness_noncalls =
        mean(vote[noncall == 1] == party_pos[noncall == 1]),
      n_party_calls = sum(party_call == 1),
      n_noncalls = sum(noncall == 0)
    ), mc]
  member_year_data <- merge(member_year_data, ld, by = "mc")
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
  member_year_data <- merge(member_year_data, ideal, by = "mc")
  member_year_data$congress <- congress
  is_orientation_correct <- member_year_data[party == "R", mean(pf_ideal)] >
    member_year_data[party == "D", mean(pf_ideal)]
  if (!is_orientation_correct) {
    member_year_data[, pf_ideal := -pf_ideal]
  }
  member_year_data[, pf_ideal := 5 + pf_ideal - mean(pf_ideal)]
  member_year_data[, pf_ideal := pf_ideal / sd(pf_ideal)]
  member_year_data[, dist_from_floor_median := abs(pf_ideal - median(pf_ideal))]
  member_year_data[party == "D", dist_from_party_median := abs(pf_ideal - median(pf_ideal))]
  member_year_data[party == "R", dist_from_party_median := abs(pf_ideal - median(pf_ideal))]
  member_year_data[, ideological_extremism := abs(pf_ideal - 5)]
  member_year_data[party == "D", ideological_extremism := -pf_ideal]
  list(member_year_data = member_year_data, fitted_emIRT = fitted_emIRT)
}
