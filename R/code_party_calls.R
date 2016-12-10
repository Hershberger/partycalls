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
#' @param type character string, one of brglm, glm, or lm; which function
#' to use for roll call-by-roll call regression
#' @param use_classification_distance logical for whether to calc
#' classification distance
#' @return rollcall object with record of classification algorithm and
#' list of classified party calls
#' @import data.table emIRT pscl
#' @export
code_party_calls <- function(rc,
  pval_threshold = 0.01, tval_threshold = 2.32, count_min = 10,
  count_max = 150, match_count_min = 150, sim_annealing = TRUE,
  random_seed = FALSE, lopside_thresh = 0.65, vote_switch_percent = 0.01,
  drop_very_lopsided_votes = TRUE, return_pvals = FALSE,
  n_iterations_for_coding = 5, use_new_match_check = TRUE,
  type = c("brglm", "lm", "glm"), use_classification_distance = FALSE)
{
  stopifnot(type %in% c("brglm", "lm", "glm"))
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
    noncalls_DT <- DT[, .(yea_perc = mean(y, na.rm = TRUE)), by = vt]
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
  countdown_started <- FALSE
  tvals <- rep(0, rc$m)
  while (counter <= count_min |
      (counter < count_max & match_counter < match_count_min)) {
    counter <- counter + 1
    record_of_coding[[counter]] <- noncalls
    old_noncalls <- noncalls
    old_switched_votes <- switched_votes
    if (use_classification_distance) {
      results <- test_classification(rc, DT, noncalls, tvals, type)
      tvals <- results$regs$t
      classification_distance <- results$classification_distance
      classification_distance_message <- paste("Classification distance: ",
        sprintf("%.03f", classification_distance), "\n")
      record_of_tvals[[counter]] <- tvals
      noncalls <- which(abs(tvals) < tval_threshold)
    } else {
      classification_distance_message <- ""
      if (return_pvals) {
        pvals <- code_party_calls_1step(rc, DT, noncalls, return_pvals, type)
        record_of_pvals[[counter]] <- pvals
        noncalls <- which(pvals > pval_threshold)
      } else {
        tvals <- code_party_calls_1step(rc, DT, noncalls, return_pvals, type)
        record_of_tvals[[counter]] <- tvals
        noncalls <- which(abs(tvals) < tval_threshold)
      }
    }
    calls <- setdiff(seq_len(rc$m), noncalls)
    if (sim_annealing == TRUE) {
      temp_switched_votes <- symdiff(noncalls, old_noncalls)
      # n_random_switches <- floor(rc$m * .2 * max(0, 1 - (abs(counter - 10) / 50))^2)
      n_random_switches <- floor(rc$m * .2 * max(0, 1 - (abs(counter) / 50))^2)
      # if (return_pvals) {
      #   probs <- abs(log(pvals) - log(pval_threshold)) ^ -.2
      #   probs[is.na(probs)] <- min(probs, na.rm = TRUE)
      #   probs <- probs / sum(probs)
      # } else {
      #   probs <- abs(log(tvals) - log(tval_threshold)) ^ -.2
      #   probs[is.na(probs)] <- min(probs, na.rm = TRUE)
      #   probs <- probs / sum(probs)
      # }

      calls_to_switch <- sample(calls, n_random_switches)
      noncalls_to_switch <- sample(noncalls, n_random_switches)
      # grays_to_make_calls <- sample(old_switched_votes, n_random_switches)
      # grays_to_make_noncalls <- setdiff(old_switched_votes, grays_to_make_calls)
      calls_to_keep <- setdiff(calls, calls_to_switch)
      # calls_to_keep <- setdiff(calls_to_keep, grays_to_make_noncalls)
      noncalls_to_keep <- setdiff(noncalls, noncalls_to_switch)
      # noncalls_to_keep <- setdiff(noncalls_to_keep, grays_to_make_calls)
      calls <- c(unique(calls_to_keep, noncalls_to_switch))
      # calls <- c(unique(calls_to_keep, noncalls_to_switch, grays_to_make_calls))
      noncalls <- c(unique(noncalls_to_keep, calls_to_switch))
      # noncalls <- c(unique(noncalls_to_keep, calls_to_switch, grays_to_make_noncalls))
    }
    switched_votes <- symdiff(noncalls, old_noncalls)
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
          # if ((length(switched_votes) < length(old_switched_votes) |
          length(switched_votes) == 0) &
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
  if (return_pvals) {
    rc$record_of_pvals <- record_of_pvals
  } else {
    rc$record_of_tvals <- record_of_tvals
  }
  rc$party_call_coding <- get_party_call_coding(rc, n_iterations_for_coding)
  rc
}
