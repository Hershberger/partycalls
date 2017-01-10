sen_special_election_dt <- fread("inst/extdata/sen_special_election_DT.csv")
setnames(sen_special_election_dt, "winner", "candname_vote_rank_1")
sen_special_election_dt[demVote > repVote, vote_vote_rank_1 := demVote]
sen_special_election_dt[demVote < repVote, vote_vote_rank_1 := repVote]
sen_special_election_dt[demVote > repVote, party_vote_rank_1 := "Democratic"]
sen_special_election_dt[demVote < repVote, party_vote_rank_1 := "Republican"]
sen_special_election_dt[, candname_vote_rank_2 := NA]
sen_special_election_dt[demVote > repVote, vote_vote_rank_2 := repVote]
sen_special_election_dt[demVote < repVote, vote_vote_rank_2 := demVote]
sen_special_election_dt[demVote > repVote, party_vote_rank_2 := "Republican"]
sen_special_election_dt[demVote < repVote, party_vote_rank_2 := "Democratic"]
sen_special_election_dt[, demVote := NULL]
sen_special_election_dt[, repVote := NULL]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "COONS",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 112 & icpsrLegis == 40916,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "GILLIBRAND",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 112 & icpsrLegis == 20735,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "MANCHIN",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 112 & icpsrLegis == 40915,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "WICKER",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 111:112 & icpsrLegis == 29534,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "BARRASSO",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 111:112 & icpsrLegis == 40707,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "TALENT",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 109 & icpsrLegis == 29369,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "MILLER",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 107:108 & icpsrLegis == 49904,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "BROWNBACK",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 105 & icpsrLegis == 29523,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "THOMPSON",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 104 & icpsrLegis == 49503,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "INHOFE",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 104 & icpsrLegis == 15424,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "FEINSTEIN",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 103 & icpsrLegis == 49300,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "AKAKA",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 102:103 & icpsrLegis == 14400,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "COATS",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 102 & icpsrLegis == 14806,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "STEWART",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 96 & icpsrLegis == 14711,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "DURENBERGER",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 96:97 & icpsrLegis == 14703,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "CONRAD",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 103 & icpsrLegis == 15502,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "WYDEN",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 105 & icpsrLegis == 14871,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

# x <- sen_special_election_dt[candname_vote_rank_1 %like% "HUTCHISON",
#   .(candname_vote_rank_1, election_date,
#     vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
#     vote_vote_rank_2, party_vote_rank_2)]
# senator_year_data[congress %in% 103 & icpsrLegis == 49306,
#   c("candname_vote_rank_1", "election_date",
#     "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
#     "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 == "BROWN (R MA)",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 111:112 & icpsrLegis == 40913,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "KIRK",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 111 & icpsrLegis == 20115,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "WOFFORD",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 103 & icpsrLegis == 49104,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "EVANS",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 99:100 & icpsrLegis == 14916,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "DURKIN",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 94:96 & icpsrLegis == 14310,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "STEVENSON",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 93 & icpsrLegis == 13102,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "STAFFORD",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 94 & icpsrLegis == 10562,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]

x <- sen_special_election_dt[candname_vote_rank_1 %like% "COVERDELL",
  .(candname_vote_rank_1, election_date,
    vote_vote_rank_1, party_vote_rank_1, candname_vote_rank_2,
    vote_vote_rank_2, party_vote_rank_2)]
senator_year_data[congress %in% 103:105 & icpsrLegis == 49301,
  c("candname_vote_rank_1", "election_date",
    "vote_vote_rank_1", "party_vote_rank_1", "candname_vote_rank_2",
    "vote_vote_rank_2", "party_vote_rank_2") := x]
