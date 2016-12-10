# Make dataset of election returns for on-cycle elections
senate_seats <- senator_year_data[, .N, .(stabb, class)][, .(stabb, class)]
setorder(senate_seats, stabb, class)
senate_seat_elections <- rbind(
  senate_seats[class == 1, .(congress = seq(91, 112, 3)), .(stabb, class)],
  senate_seats[class == 2, .(congress = seq(92, 110, 3)), .(stabb, class)],
  senate_seats[class == 3, .(congress = seq(90, 111, 3)), .(stabb, class)])
senate_seat_elections[, election_year := calc_year(congress)]

# Fetch David Leip's data
#source("package/get_election_atlas_data.R")
load("inst/extdata/senate_elections.Rdata")
setnames(senate_elections, c("parties", "year"), c("party", "election_year"))
senate_elections_vote_rank_1 <- senate_elections[vote_rank == 1]
setnames(senate_elections_vote_rank_1,
  c("candname", "vote", "votepct", "party"),
  paste0(c("candname", "vote", "votepct", "party"), "_vote_rank_1"))
senate_elections_vote_rank_2 <- senate_elections[vote_rank == 2,
  .(fips, election_year, candname, vote, votepct, party)]
setnames(senate_elections_vote_rank_2,
  c("candname", "vote", "votepct", "party"),
  paste0(c("candname", "vote", "votepct", "party"), "_vote_rank_2"))
senate_elections <- merge(senate_elections_vote_rank_1,
  senate_elections_vote_rank_2, by = c("fips", "election_year"))
senate_seat_elections <- merge(senate_seat_elections,
  senate_elections, by = c("stabb", "election_year", "class"),
  all.x = TRUE)
senate_seat_elections[, fips := NULL]
senate_seat_elections[, vote_rank := NULL]
senate_seat_elections[, icpsr_state := NULL]
senate_seat_elections[, statename := NULL]
senate_seat_elections[, congress1 := NULL]
senate_seat_elections[, congress2 := NULL]
senate_seat_elections[, congress3 := NULL]
senate_seat_elections[, votepct_vote_rank_1 := NULL]
senate_seat_elections[, votepct_vote_rank_2 := NULL]

# break into two parts
# _1 is the set of already populated rows
# _2 is the set remaining to be populated
senate_seat_elections_1 <- senate_seat_elections[!is.na(candname_vote_rank_1)]
senate_seat_elections_2 <- senate_seat_elections[is.na(candname_vote_rank_1)]
senate_seat_elections_2 <- senate_seat_elections_2[, .(stabb, election_year,
  class)]

# Fetch data hand coded from house.gov pdf's
senate_elections_eh <- fread("inst/extdata/sen_election_DT.csv")
setnames(senate_elections_eh, "state", "stabb")
senate_elections_eh[election_year %in% seq(1964, 2012, 6), class := 1]
senate_elections_eh[election_year %in% seq(1966, 2014, 6), class := 2]
senate_elections_eh[election_year %in% seq(1968, 2016, 6), class := 3]
senate_elections_eh[, congress := calc_congress(election_year)]
setnames(senate_elections_eh, "winner", "candname_vote_rank_1")
senate_elections_eh[demVote > repVote, vote_vote_rank_1 := demVote]
senate_elections_eh[demVote < repVote, vote_vote_rank_1 := repVote]
senate_elections_eh[demVote > repVote, party_vote_rank_1 := "Democratic"]
senate_elections_eh[demVote < repVote, party_vote_rank_1 := "Republican"]
senate_elections_eh[, candname_vote_rank_2 := NA]
senate_elections_eh[demVote > repVote, vote_vote_rank_2 := repVote]
senate_elections_eh[demVote < repVote, vote_vote_rank_2 := demVote]
senate_elections_eh[demVote > repVote, party_vote_rank_2 := "Republican"]
senate_elections_eh[demVote < repVote, party_vote_rank_2 := "Democratic"]
senate_elections_eh[, demVote := NULL]
senate_elections_eh[, repVote := NULL]
setcolorder(senate_elections_eh, names(senate_seat_elections_1))

# fill out _2 with hand-coded house.gov data
senate_seat_elections_2 <- merge(senate_seat_elections_2, senate_elections_eh,
  by = c("stabb", "election_year", "class"), all.x = TRUE)

# rbind _1 and _2 together
senate_seat_elections <- rbind(senate_seat_elections_1, senate_seat_elections_2)
senate_seat_elections[, congress := NULL]