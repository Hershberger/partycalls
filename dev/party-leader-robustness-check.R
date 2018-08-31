library(partycalls)
library(data.table)
library(texreg)
dem_maj <- c(93:103, 110:111) - 92
rep_maj <- c(104:109, 112) - 92

# code responsiveness variables based on party leader' positions for dems
dem_coding <- rbindlist(lapply(1:20, function(i) {
  rc <- house_party_calls[[i]]
  if (i %in% dem_maj) {
    leader_icpsr <- house_data[congress == 92 + i & maj_leader == 1, icpsrLegis]
  } else {
    leader_icpsr <- house_data[congress == 92 + i & min_leader == 1, icpsrLegis]
  }
  # get leaders' votes
  leader_rows <- which(rc$legis.data$icpsrLegis %in% leader_icpsr)
  leader_votes <- rc$votes[leader_rows, ]
  # only keep votes for which leaders were unanimous
  votes_to_keep <- which(sapply(1:ncol(leader_votes), function(j) {
    length(unique(leader_votes[, j][leader_votes[, j] %in% c(1, -1)])) == 1
  }))
  # get party call coding, limit to votes_to_keep
  party_calls <- rc$party_call_coding[, which(coding == "party call")]
  non_party_calls <- rc$party_call_coding[, which(coding == "noncall")]
  party_calls <- intersect(votes_to_keep, party_calls)
  non_party_calls <- intersect(votes_to_keep, non_party_calls)
  # code positions for party calls
  party_calls_positions <- sapply(party_calls, function(j) {
    unique(leader_votes[, j][leader_votes[, j] %in% c(1, -1)])
  })
  # code positions for non-calls
  non_party_calls_positions <- sapply(non_party_calls, function(j) {
    unique(leader_votes[, j][leader_votes[, j] %in% c(1, -1)])
  })
  # calculate responsiveness rates for dems
  data.table(
    congress = 92 + i,
    party = rc$legis.data$party,
    icpsrLegis = rc$legis.data$icpsrLegis,
    responsiveness_to_party_calls_leader =
      sapply(1:nrow(rc$legis.data), function(j) {
        100 * sum(rc$votes[j, party_calls] == party_calls_positions) /
          sum(rc$votes[j, party_calls] %in% c(1, -1))
    }),
    baseline_rate_leader =
      sapply(1:nrow(rc$legis.data), function(j) {
        100 * sum(rc$votes[j, non_party_calls] == non_party_calls_positions) /
          sum(rc$votes[j, non_party_calls] %in% c(1, -1))
      })
  )[party == "D"]
}))

# code responsiveness variables based on party leader' positions for reps
rep_coding <- rbindlist(lapply(1:20, function(i) {
  rc <- house_party_calls[[i]]
  if (i %in% rep_maj) {
    leader_icpsr <- house_data[congress == 92+i & maj_leader == 1, icpsrLegis]
  } else {
    leader_icpsr <- house_data[congress == 92+i & min_leader == 1, icpsrLegis]
  }
  leader_rows <- which(rc$legis.data$icpsrLegis %in% leader_icpsr)
  leader_votes <- rc$votes[leader_rows, ]
  votes_to_keep <- which(sapply(1:ncol(leader_votes), function(j) {
    length(unique(leader_votes[, j][leader_votes[, j] %in% c(1, -1)])) == 1
  }))
  party_calls <- rc$party_call_coding[, which(coding == "party call")]
  non_party_calls <- rc$party_call_coding[, which(coding == "noncall")]
  party_calls <- intersect(votes_to_keep, party_calls)
  non_party_calls <- intersect(votes_to_keep, non_party_calls)
  party_calls_positions <- sapply(party_calls, function(j) {
    unique(leader_votes[, j][leader_votes[, j] %in% c(1, -1)])
  })
  non_party_calls_positions <- sapply(non_party_calls, function(j) {
    unique(leader_votes[, j][leader_votes[, j] %in% c(1, -1)])
  })
  data.table(
    congress = 92 + i,
    party = rc$legis.data$party,
    icpsrLegis = rc$legis.data$icpsrLegis,
    responsiveness_to_party_calls_leader =
      sapply(1:nrow(rc$legis.data), function(j) {
        100 * sum(rc$votes[j, party_calls] == party_calls_positions) /
          sum(rc$votes[j, party_calls] %in% c(1, -1))
      }),
    baseline_rate_leader =
      sapply(1:nrow(rc$legis.data), function(j) {
        100 * sum(rc$votes[j, non_party_calls] == non_party_calls_positions) /
          sum(rc$votes[j, non_party_calls] %in% c(1, -1))
      })
  )[party == "R"]
}))

# merge new responsiveness rates with main dataset
house_data <- rbind(
  merge(partycalls::house_data, dem_coding, by = c("congress", "icpsrLegis")),
  merge(partycalls::house_data, rep_coding, by = c("congress", "icpsrLegis")))

# drop observations with missing responsiveness rates
house_data <- na.omit(house_data)

model <- lm(responsiveness_to_party_calls_leader ~ ideological_extremism +
    baseline_rate_leader + vote_share + pres_vote_share + leader + chair +
    power_committee + best_committee + female + african_american + latino +
    south + seniority + freshman, house_data)
ses <- robust_se(model,
  house_data[, congress],
  house_data[, icpsrLegis])
pvals <- 2 * (1 - pnorm(abs(coef(model) / ses)))
screenreg(model, override.se = ses, override.pvalues = pvals)
