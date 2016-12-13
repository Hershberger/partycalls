
#' Make dataset at level of MC/year
#'
#' For a single congress, assemble data for analysis
#' @param congress congress ID number
#' @param roll_calls_object_list rc a list of rollcall objects with classified
#' party calls
#' @return data.table with party-free ideal points
#' @import data.table emIRT pscl
#' @export
make_member_year_data <- function(congress, roll_calls_object_list,
  chamber = "house")
{
  if (chamber == "house"){
    rc <- roll_calls_object_list[[paste0("hou", congress)]]
  } else if (chamber == "senate") {
    rc <- roll_calls_object_list[[paste0("sen", congress)]]
  }
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
