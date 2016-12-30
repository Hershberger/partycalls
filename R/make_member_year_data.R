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
  # set up data to merge
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
  # make responsiveness data
  party_call_coding <- rc$party_call_coding$coding
  gray_votes <- votes$vote_id[which(party_call_coding == "gray")]
  party_calls <- votes$vote_id[which(party_call_coding == "party call")]
  noncalls <- votes$vote_id[which(party_call_coding == "noncall")]
  votes[, gray := as.numeric(vote_id %in% gray_votes)]
  votes[, party_call := as.numeric(vote_id %in% party_calls)]
  votes[, noncall := as.numeric(vote_id %in% noncalls)]
  votes[, party_yea_rate := sum(vote == 1) / sum(vote %in% c(1, -1)),
    .(vote_id, party)]
  votes[, party_pos :=
      as.numeric(party_yea_rate > .5) - as.numeric(party_yea_rate < .5)]
  responsiveness_data <- votes[vote %in% c(1, -1) & party_pos != 0,
    .(
      responsiveness_party_calls =
        mean(vote[party_call == 1] == party_pos[party_call == 1]),
      responsiveness_noncalls =
        mean(vote[noncall == 1] == party_pos[noncall == 1]),
      n_party_calls = sum(party_call == 1),
      n_noncalls = sum(noncall == 1)
    ), mc]
  # make ideal points
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
  # merge data sets
  member_year_data <- merge(responsiveness_data, ld, by = "mc")
  member_year_data <- merge(member_year_data, ideal, by = "mc")
  member_year_data$congress <- congress
  # check ideal point orientation, switch if necessary, scale, calc extremism
  is_orientation_correct <- member_year_data[,
    mean(pf_ideal[party == "R"]) > mean(pf_ideal[party == "D"])]
  if (!is_orientation_correct) {
    member_year_data[, pf_ideal := -pf_ideal]
  }
  member_year_data[, pf_ideal := pf_ideal / sd(pf_ideal)]
  member_year_data[, pf_ideal := pf_ideal - mean(pf_ideal)]
  member_year_data[, ideological_extremism := -pf_ideal]
  member_year_data[party == "R", ideological_extremism := pf_ideal]
  list(member_year_data = member_year_data, fitted_emIRT = fitted_emIRT)
}
