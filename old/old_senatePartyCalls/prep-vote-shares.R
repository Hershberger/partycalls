# munging the vote shares
load("inst/extdata/senate_elections.Rdata")
vote_share_dem <- senate_elections[parties == "Democratic",
  .(dem_vote = vote), .(stabb, year)]
vote_share_rep <- senate_elections[parties == "Republican",
  .(rep_vote = vote), .(stabb, year)]
vote_share_dt <- merge(vote_share_dem, vote_share_rep, by = c("stabb", "year"),
  all = TRUE, allow.cartesian = TRUE)
eh_vote_share_dt <- fread("inst/extdata/sen_election_DT.csv")
setnames(eh_vote_share_dt, c("state", "election_year"), c("stabb", "year"))
vote_share_merge <-
  merge(vote_share_dt, eh_vote_share_dt, by = c("stabb", "year"),
    all = TRUE, allow.cartesian = TRUE)
vote_share_merge[dem_vote == demVote & rep_vote == repVote]