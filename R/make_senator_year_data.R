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
