#' Calculate party vote rates for legislators
#'
#' Party vote rates are the fraction of times a legislator votes with her
#' party on votes that pit majorities of the two parties against each other.
#' @param congress_index integer from 1 to 20
#' @param roll_call_list list of roll call objects with votes coded as 1 and -1
#' @return data.table with congress, icpsrLegis, and party vote rate
#' @export
#' @import data.table
calc_party_vote_rates <- function(congress_index, roll_call_list)
{
  n <- roll_call_list[[congress_index]]$n
  m <- roll_call_list[[congress_index]]$m
  votes <- roll_call_list[[congress_index]]$votes
  reps <- roll_call_list[[congress_index]]$legis.data$party == "R"
  dems <- roll_call_list[[congress_index]]$legis.data$party == "D"
  # find majority Republican position
  n_rep_yeas <- apply(votes[reps, ], 2, function(x) sum(x == 1))
  n_rep_nays <- apply(votes[reps, ], 2, function(x) sum(x == -1))
  rep_pos <- rep(1, length(n_rep_yeas))
  rep_pos[n_rep_yeas < n_rep_nays] <- -1
  rep_pos[n_rep_yeas == n_rep_nays] <- 0
  rep_pos_mat <- matrix(rep_pos, nrow = n, ncol = m, byrow = TRUE)
  # find majority Democrat position
  n_dem_yeas <- apply(votes[dems, ], 2, function(x) sum(x == 1))
  n_dem_nays <- apply(votes[dems, ], 2, function(x) sum(x == -1))
  dem_pos <- rep(1, length(n_dem_yeas))
  dem_pos[n_dem_yeas < n_dem_nays] <- -1
  dem_pos[n_dem_yeas == n_dem_nays] <- 0
  dem_pos_mat <- matrix(dem_pos, nrow = n, ncol = m, byrow = TRUE)
  # make matrix of party positions
  party_pos_mat <- matrix(NA, nrow = n, ncol = m)
  party_pos_mat[reps, ] <- rep_pos_mat[reps, ]
  party_pos_mat[dems, ] <- dem_pos_mat[dems, ]
  # make matrix of voting with party positions
  vote_with_party <- party_pos_mat == votes
  vote_with_party[votes == 9 | votes == 0] <- NA
  # identify party votes as the times when majorities of parties voted against
  #   each other
  party_votes <- which(rep_pos != dem_pos & rep_pos != 0 & dem_pos != 0)
  vote_with_party <- vote_with_party[, party_votes]
  data.table(congress = congress_index + 92,
    icpsrLegis = roll_call_list[[congress_index]]$legis.data$icpsrLegis,
    party_vote_rate = 100 * rowMeans(vote_with_party, na.rm = TRUE))
}



#' Identify party votes
#'
#' Party votes are those that pit majorities of the two parties against each
#' other.
#' @param congress_index integer from 1 to 20
#' @param roll_call_list list of roll call objects with votes coded as 1 and -1
#' @return integer vector of votes in the roll call object that are party votes
#' @export
#' @import data.table
identify_party_votes <- function(congress_index, roll_call_list)
{
  n <- roll_call_list[[congress_index]]$n
  m <- roll_call_list[[congress_index]]$m
  votes <- roll_call_list[[congress_index]]$votes
  reps <- roll_call_list[[congress_index]]$legis.data$party == "R"
  dems <- roll_call_list[[congress_index]]$legis.data$party == "D"
  # find majority Republican position
  n_rep_yeas <- apply(votes[reps, ], 2, function(x) sum(x == 1))
  n_rep_nays <- apply(votes[reps, ], 2, function(x) sum(x == -1))
  rep_pos <- rep(1, length(n_rep_yeas))
  rep_pos[n_rep_yeas < n_rep_nays] <- -1
  rep_pos[n_rep_yeas == n_rep_nays] <- 0
  # find majority Democrat position
  n_dem_yeas <- apply(votes[dems, ], 2, function(x) sum(x == 1))
  n_dem_nays <- apply(votes[dems, ], 2, function(x) sum(x == -1))
  dem_pos <- rep(1, length(n_dem_yeas))
  dem_pos[n_dem_yeas < n_dem_nays] <- -1
  dem_pos[n_dem_yeas == n_dem_nays] <- 0
  # identify party votes as the times when majorities of parties voted against
  #   each other
  which(rep_pos != dem_pos & rep_pos != 0 & dem_pos != 0)
}





#' Calculate `vote with party' rates for legislators
#'
#' `Vote with party` rates are the fraction of times a legislator votes with her
#' party on all votes.
#' @param congress_index integer from 1 to 20
#' @param roll_call_list list of roll call objects with votes coded as 1 and -1
#' @return data.table with congress, icpsrLegis, and party vote rate
#' @export
#' @import data.table
calc_vote_with_party_rates <- function(congress_index, roll_call_list)
{
  n <- roll_call_list[[congress_index]]$n
  m <- roll_call_list[[congress_index]]$m
  votes <- roll_call_list[[congress_index]]$votes
  reps <- roll_call_list[[congress_index]]$legis.data$party == "R"
  dems <- roll_call_list[[congress_index]]$legis.data$party == "D"
  # find majority Republican position
  n_rep_yeas <- apply(votes[reps, ], 2, function(x) sum(x == 1))
  n_rep_nays <- apply(votes[reps, ], 2, function(x) sum(x == -1))
  rep_pos <- rep(1, length(n_rep_yeas))
  rep_pos[n_rep_yeas < n_rep_nays] <- -1
  rep_pos[n_rep_yeas == n_rep_nays] <- 0
  rep_pos_mat <- matrix(rep_pos, nrow = n, ncol = m, byrow = TRUE)
  # find majority Democrat position
  n_dem_yeas <- apply(votes[dems, ], 2, function(x) sum(x == 1))
  n_dem_nays <- apply(votes[dems, ], 2, function(x) sum(x == -1))
  dem_pos <- rep(1, length(n_dem_yeas))
  dem_pos[n_dem_yeas < n_dem_nays] <- -1
  dem_pos[n_dem_yeas == n_dem_nays] <- 0
  dem_pos_mat <- matrix(dem_pos, nrow = n, ncol = m, byrow = TRUE)
  # make matrix of party positions
  party_pos_mat <- matrix(NA, nrow = n, ncol = m)
  party_pos_mat[reps, ] <- rep_pos_mat[reps, ]
  party_pos_mat[dems, ] <- dem_pos_mat[dems, ]
  # make matrix of voting with party positions
  vote_with_party <- party_pos_mat == votes
  vote_with_party[votes == 9 | votes == 0] <- NA
  data.table(congress = congress_index + 92,
    icpsrLegis = roll_call_list[[congress_index]]$legis.data$icpsrLegis,
    vote_with_party_rate = 100 * rowMeans(vote_with_party, na.rm = TRUE))
}


#' Calculate non-party vote rates for legislators
#'
#' Party vote rates are the fraction of times a legislator votes with her
#' party on votes that do not pit majorities of the two parties against each other.
#' @param congress_index integer from 1 to 20
#' @param roll_call_list list of roll call objects with votes coded as 1 and -1
#' @return data.table with congress, icpsrLegis, and party vote rate
#' @export
#' @import data.table
calc_nonparty_vote_rates <- function(congress_index, roll_call_list)
{
  n <- roll_call_list[[congress_index]]$n
  m <- roll_call_list[[congress_index]]$m
  votes <- roll_call_list[[congress_index]]$votes
  reps <- roll_call_list[[congress_index]]$legis.data$party == "R"
  dems <- roll_call_list[[congress_index]]$legis.data$party == "D"
  # find majority Republican position
  n_rep_yeas <- apply(votes[reps, ], 2, function(x) sum(x == 1))
  n_rep_nays <- apply(votes[reps, ], 2, function(x) sum(x == -1))
  rep_pos <- rep(1, length(n_rep_yeas))
  rep_pos[n_rep_yeas < n_rep_nays] <- -1
  rep_pos[n_rep_yeas == n_rep_nays] <- 0
  rep_pos_mat <- matrix(rep_pos, nrow = n, ncol = m, byrow = TRUE)
  # find majority Democrat position
  n_dem_yeas <- apply(votes[dems, ], 2, function(x) sum(x == 1))
  n_dem_nays <- apply(votes[dems, ], 2, function(x) sum(x == -1))
  dem_pos <- rep(1, length(n_dem_yeas))
  dem_pos[n_dem_yeas < n_dem_nays] <- -1
  dem_pos[n_dem_yeas == n_dem_nays] <- 0
  dem_pos_mat <- matrix(dem_pos, nrow = n, ncol = m, byrow = TRUE)
  # make matrix of party positions
  party_pos_mat <- matrix(NA, nrow = n, ncol = m)
  party_pos_mat[reps, ] <- rep_pos_mat[reps, ]
  party_pos_mat[dems, ] <- dem_pos_mat[dems, ]
  # make matrix of voting with party positions
  vote_with_party <- party_pos_mat == votes
  vote_with_party[votes == 9 | votes == 0] <- NA
  # identify party votes as the times when majorities of parties voted against
  #   each other
  nonparty_votes <- which(rep_pos == dem_pos & rep_pos != 0 & dem_pos != 0)
  vote_with_party <- vote_with_party[, nonparty_votes]
  data.table(congress = congress_index + 92,
    icpsrLegis = roll_call_list[[congress_index]]$legis.data$icpsrLegis,
    nonparty_vote_rate = 100 * rowMeans(vote_with_party, na.rm = TRUE))
}
