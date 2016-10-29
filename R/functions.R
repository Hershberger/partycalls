

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
#' To be used inside a call to code_party_calls_1step. This function is used to
#' perform a number of tasks in the purpose of determining the influence of the
#' party on a given vote. This is accomplished by regressing votes on members'
#' party affiliation and ideology. Before this happens, however, all votes which
#' either received only `yeas' and `nays' are assigned non-values in place of
#' estimates. Further, votes which strictly follow party lines are assigned
#' estimates which guarantee they will be coded as party calls under any model
#' specification to avoid separation in the model. Finally, the votes left are
#' either put through a bias-reduced logit or ordinary least squares model as
#' per user selected parameters. The default setting of the function is to use
#' the bias-reduced logit.
#' @param .SD subset of a data.table of roll call votes, with a column for party
#' labels
#' @param type character string, one of brglm, glm, or lm; which function
#' to use for roll call-by-roll call regression
#' @return list of coefficient, standard error, t value, and p value for the
#' coefficient on party
#' @export
#' @importFrom brglm brglm
test_rollcall <- function(.SD, type = c("brglm", "lm", "glm"))
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
    if (type == "brglm") {
      m <- brglm::brglm(y ~ party + x, data = .SD, family = binomial)
      suppressWarnings(summ <- summary(m)$coef["partyR", ])
      list(b = summ["Estimate"], se = summ["Std. Error"],
        t = summ["z value"], p = summ["Pr(>|z|)"])
    } else if (type == "glm") {
      suppressWarnings(m <- glm(y ~ party + x, data = .SD, family = binomial))
      suppressWarnings(summ <- summary(m)$coef["partyR", ])
      list(b = summ["Estimate"], se = summ["Std. Error"],
        t = summ["z value"], p = summ["Pr(>|z|)"])
    } else {
      m <- lm(y ~ party + x, data = .SD)
      suppressWarnings(summ <- summary(m)$coef["partyR", ])
      list(b = summ["Estimate"], se = summ["Std. Error"],
        t = summ["t value"], p = summ["Pr(>|t|)"])
    }
  }
}

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
    if (counter >= 5 & sim_annealing == TRUE) {
      temp_switched_votes <- symdiff(noncalls, old_noncalls)
      n_random_switches <- floor(rc$m * .2 * max(0, 1 - counter / 50) ^ 2)
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
      calls_to_keep <- setdiff(calls, calls_to_switch)
      noncalls_to_keep <- setdiff(noncalls, noncalls_to_switch)
      calls <- c(calls_to_keep, noncalls_to_switch)
      noncalls <- c(noncalls_to_keep, calls_to_switch)
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

#' Code party calls
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that was always a party call as a party call, any vote that was always
#' a noncall as a noncall, and the remainder as gray votes.
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return data.table with two columns, voteno and coding
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

#' Code party calls
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that was always a party call as a party call
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return character string of vote IDs, based on KH Data index
#' @export
get_party_calls <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_ever_classied_as_noncalls <- Reduce(union, record_of_coding)
  party_calls <- setdiff(seq_len(rc$m), all_votes_ever_classied_as_noncalls)
  paste("Vote", party_calls)
  colnames(rc$votes)[party_calls]
}

#' Code noncalls
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that was always a noncall as a noncall.
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return character string of vote IDs, based on KH Data index
#' @export
get_noncalls <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_always_classied_as_noncalls <- Reduce(intersect, record_of_coding)
  noncalls <- all_votes_always_classied_as_noncalls
  colnames(rc$votes)[noncalls]
}

#' Code gray votes
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that wasn't always a party call or always a noncall as a gray vote.
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return character string of vote IDs, based on KH Data index
#' @export
get_gray_votes <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_ever_classied_as_calls <- Reduce(union, record_of_coding)
  all_votes_always_classied_as_calls <- Reduce(intersect, record_of_coding)
  gray_votes <- setdiff(all_votes_ever_classied_as_calls,
    all_votes_always_classied_as_calls)
  colnames(rc$votes)[gray_votes]
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
  gray_votes <- get_gray_votes(rc)
  party_calls <- get_party_calls(rc)
  noncalls <- get_noncalls(rc)
  votes[, gray := as.numeric(vote_id %in% gray_votes)]
  votes[, party_call := as.numeric(vote_id %in% party_calls)]
  votes[, noncall := as.numeric(vote_id %in% noncalls)]
  votes[, party_yea_rate := sum(vote == 1) / sum(vote %in% c(1, -1)),
    .(vote_id, party)]
  votes[, party_pos :=
      as.numeric(party_yea_rate > .5) - as.numeric(party_yea_rate < .5)]
  new_member_year_data <- votes[vote %in% c(1, -1) & party_pos != 0,
    .(
      responsiveness_party_calls =
        mean(vote[party_call == 1] == party_pos[party_call == 1]),
      responsiveness_noncalls =
        mean(vote[noncall == 1] == party_pos[noncall == 1]),
      n_party_calls = sum(party_call == 1),
      n_noncalls = sum(noncall == 1)
    ), mc]

  old_member_year_data <- votes[vote %in% c(1, -1) & party_pos != 0,
    .(
      n_match_party_position_party_calls =
        sum(vote[party_call == 1] == party_pos[party_call == 1]),
      n_match_party_position_noncalls =
        sum(vote[noncall == 1] == party_pos[noncall == 1]),
      n_party_calls = sum(party_call == 1),
      n_noncalls = sum(noncall == 1)
    ), mc]

  member_year_data <- copy(old_member_year_data)
  member_year_data[, responsiveness_party_calls :=
      n_match_party_position_party_calls / n_party_calls]
  member_year_data[, responsiveness_noncalls :=
      n_match_party_position_noncalls / n_noncalls]

  member_year_data <- merge(member_year_data, ld, by = "mc")
  rc_noncalls <- rc
  votes_to_keep <- which(colnames(rc_noncalls$votes) %in% noncalls)
  rc_noncalls$votes <- rc_noncalls$votes[, votes_to_keep]
  rc_noncalls$m <- ncol(rc_noncalls$votes)
  p <- emIRT::makePriors(rc_noncalls$n, rc_noncalls$m, 1)
  s <- emIRT::getStarts(rc_noncalls$n, rc_noncalls$m, 1)
  sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
    "NUL"
  } else {
    "/dev/null"
  }
  sink(sink_target)
  fitted_emIRT <- emIRT::binIRT(.rc = rc_noncalls, .starts = s, .priors = p,
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
  member_year_data[, pf_ideal := pf_ideal / sd(pf_ideal)]
  member_year_data[, pf_ideal := 5 + pf_ideal - mean(pf_ideal)]
  member_year_data[,
    dist_from_floor_median := abs(pf_ideal - median(pf_ideal))]
  member_year_data[party == "D",
    dist_from_party_median := abs(pf_ideal - median(pf_ideal))]
  member_year_data[party == "R",
    dist_from_party_median := abs(pf_ideal - median(pf_ideal))]
  member_year_data[, ideological_extremism := -pf_ideal]
  member_year_data[party == "R", ideological_extremism := pf_ideal]
  list(member_year_data = member_year_data, fitted_emIRT = fitted_emIRT)
}
