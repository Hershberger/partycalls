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
#' avoid the algorithm from staying at local maxima.
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
#' @param type character string, one of brglm, glm, or lm; which function
#' to use for roll call-by-roll call regression
# @param use_classification_distance logical for whether to calc
# classification distance
#' @param use_noncalls_for_ideal_point_estimation logical for whether
#' to use all noncalls for ideal point estimation
#' @return rollcall object with record of classification algorithm and
#' list of classified party calls
#' @import data.table
#' @importFrom pscl dropRollCall
#' @importFrom emIRT convertRC
#' @export
code_party_calls <- function(rc,
  sim_annealing = FALSE,
  hybrid = FALSE,
  reassign_flip_flop = FALSE,
  use_new_match_check = FALSE,
  count_max = 100,
  match_count_min = 15,
  count_min = 15,
  pval_threshold,
  tval_threshold,
  vote_switch_percent = 0.01,
  return_pvals = TRUE,
  n_iterations_for_coding = 5,
  random_seed = FALSE,
  semi_random_seed = FALSE,
  initial_vote_switch_pct = 0,
  drop_very_lopsided_votes = TRUE,
  type = "brglm",
  temperature_function = function(counter, n_votes)
    floor(n_votes * .2 * max(0, 1 - (abs(counter - 10) / 50)) ^ 2),
  lopside_thresh = 0.65,
  use_noncalls_for_ideal_point_estimation = TRUE)
{
  stopifnot(type %in% c("brglm", "lm", "glm"))
  rc <- pscl::dropRollCall(rc, dropList = alist(dropLegis = state == "USA"))
  if (drop_very_lopsided_votes) {
    rc <- pscl::dropRollCall(rc, dropList = alist(lop = 0))
  }
  rc <- emIRT::convertRC(rc, type = "binIRT")
  DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  DT$y <- as.vector(rc$votes)
  DT$party <- rc$legis.data$party
  DT[y %in% c(0, 9), y:= NA]
  DT[y == -1, y:= 0]
  if (random_seed) {
    noncalls <- sample(rc$m, floor(.5 * rc$m))
  } else {
    stopifnot(lopside_thresh > 0 & lopside_thresh < 1 & lopside_thresh != .5)
    LB <- min(1 - lopside_thresh, lopside_thresh)
    UB <- max(1 - lopside_thresh, lopside_thresh)
    noncalls_DT <- DT[, .(yea_perc = mean(y, na.rm = TRUE)), by = vt]
    noncalls_start <- which(noncalls_DT[, yea_perc <= LB | UB <= yea_perc])
  if (semi_random_seed) {
    stopifnot(initial_vote_switch_pct >= 0 & initial_vote_switch_pct <= 1)
    calls_start <- which(noncalls_DT[, yea_perc > LB & UB > yea_perc])
    lopsided_calls <- sample(noncalls_start,
      floor((length(noncalls_start) * initial_vote_switch_pct)))
    close_noncalls <- sample(calls_start,
      floor((length(calls_start) * initial_vote_switch_pct)))
    noncalls_start <- setdiff(noncalls_start, lopsided_calls)
    noncalls <- c(noncalls_start, close_noncalls)
  } else {
    noncalls <- noncalls_start
  } }
  switched_votes <- seq_len(rc$m)
  match_counter <- 0
  counter <- 0
  record_of_coding <- list()
  record_of_ideals <- list()
  match_switch <- FALSE # for old match checking procedure
  record_of_pvals <- list()
  record_of_tvals <- list()
  countdown_started <- FALSE
  tvals <- rep(0, rc$m)
  flip_flop_votes <- c()
  while (counter <= count_min |
      (counter < count_max & match_counter < match_count_min)) {
    counter <- counter + 1
    record_of_coding[[counter]] <- noncalls
    old_noncalls <- noncalls
    old_switched_votes <- switched_votes
    classification_distance_message <- NULL
    if (use_noncalls_for_ideal_point_estimation) {
      record <- code_party_calls_1step(rc, DT, noncalls, return_pvals, type)
    } else { # use only noncalls - any flip flop votes
      vote_for_ideal_points <- setdiff(noncalls, flip_flop_votes)
      record <- code_party_calls_1step(rc, DT, vote_for_ideal_points,
        return_pvals, type)
    }
    record_of_ideals[[counter]] <- record$ideal
    record_of_pvals[[counter]] <- record$pvals
    record_of_tvals[[counter]] <- record$tvals
    if (return_pvals) {
      noncalls <- which(record$pvals > pval_threshold)
    } else {
      noncalls <- which(abs(record$tvals) < tval_threshold)
    }
    calls <- setdiff(seq_len(rc$m), noncalls)
    if (sim_annealing == TRUE | hybrid == TRUE) {
      temp_switched_votes <- symdiff(noncalls, old_noncalls)
      n_random_switches <- temperature_function(counter, rc$m)
      calls_to_switch <- sample(calls, min(length(calls), n_random_switches))
      noncalls_to_switch <- sample(noncalls, min(length(noncalls),
        n_random_switches))
      calls_to_keep <- setdiff(calls, calls_to_switch)
      noncalls_to_keep <- setdiff(noncalls, noncalls_to_switch)
      calls <- c(unique(calls_to_keep, noncalls_to_switch))
      noncalls <- c(unique(noncalls_to_keep, calls_to_switch))
    }
    switched_votes <- symdiff(noncalls, old_noncalls)
    # randomly reassign flip flop votes from noncalls list
    if (reassign_flip_flop == TRUE | (hybrid == TRUE & counter > 30)) {
      flip_flop_votes <- intersect(switched_votes, old_switched_votes)
      calls_to_keep <- setdiff(calls, flip_flop_votes)
      noncalls_to_keep <- setdiff(noncalls, flip_flop_votes)
      flip_flop_calls <- sample(flip_flop_votes,
        floor(length(flip_flop_votes) / 2))
      flip_flop_noncalls <- setdiff(flip_flop_votes, flip_flop_calls)
      calls <- c(calls_to_keep, flip_flop_calls)
      noncalls <- c(noncalls_to_keep, flip_flop_noncalls)
    }
    countdown <- ""
    if (use_new_match_check & counter > count_min) {
      if (length(switched_votes) <= vote_switch_percent * rc$m) {
        match_counter <- match_counter + 1
        countdown_started <- TRUE
        countdown <- paste0("(", match_count_min -  match_counter,
          " iterations left)")
      } else if (countdown_started) {
        match_counter <- 0
        countdown_started <- FALSE
        countdown <- "(countdown started over)"
      } else {
        match_counter <- 0
      }
    } else {
      if ((length(switched_votes) > length(old_switched_votes) |
          length(switched_votes) <= 5) &
          counter > count_min) {
        match_switch <- TRUE
      }
    }
    if (match_switch) {
      match_counter <- match_counter + 1
      countdown <- paste0("(", match_count_min -  match_counter,
        " iterations left)")
    }
    cat("Iteration", counter, "had", length(switched_votes), "out of", rc$m,
      "switched votes", countdown, "\n", classification_distance_message)
  }
  rc$party_calls <- seq_len(rc$m)[-noncalls]
  rc$record_of_coding <- record_of_coding
  rc$record_of_ideals <- record_of_ideals
  rc$record_of_pvals <- record_of_pvals
  rc$record_of_tvals <- record_of_tvals
  rc$party_call_coding <- get_party_call_coding(rc, n_iterations_for_coding)
  rc
}
