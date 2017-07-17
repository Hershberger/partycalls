library(partycalls)
library(texreg)
library(data.table)
options(stringsAsFactors = FALSE)

# load data for comparison
load("data/senator_year_data_emIRT_only.RData")
emIRT_data <- data.table(icpsrLegis = senator_year_data$icpsrLegis,
  congress = senator_year_data$congress,
  emIRT_ideal = senator_year_data$party_free_ideal_point,
  emIRT_extreme = senator_year_data$ideological_extremism,
  emIRT_response_calls = senator_year_data$responsiveness_party_calls,
  emIRT_response_noncalls = senator_year_data$responsiveness_noncalls,
  emIRT_voteshare = senator_year_data$vote_share,
  emIRT_votepct = senator_year_data$votepct,
  emIRT_female = senator_year_data$female,
  emIRT_latino = senator_year_data$latino,
  emIRT_afam = senator_year_data$afam,
  emIRT_south = senator_year_data$south13,
  emIRT_election = senator_year_data$up_for_reelection,
  emIRT_senior = senator_year_data$seniority,
  emIRT_retiree = senator_year_data$retiree,
  emIRT_best_committee = senator_year_data$best_committee,
  emIRT_leader = senator_year_data$leader,
  emIRT_power_com = senator_year_data$power_committee,
  emIRT_com_chair = senator_year_data$chair,
  emIRT_pres = senator_year_data$pres_vote_share,
  caucus = senator_year_data$caucus)

load("data/senator_year_data_hybrid.RData")
hybrid_data <- data.table(icpsrLegis = senator_year_data$icpsrLegis,
  congress = senator_year_data$congress,
  hybrid_ideal = senator_year_data$party_free_ideal_point,
  hybrid_extreme = senator_year_data$ideological_extremism,
  hybrid_response_calls = senator_year_data$responsiveness_party_calls,
  hybrid_response_noncalls = senator_year_data$responsiveness_noncalls,
  hybrid_voteshare = senator_year_data$vote_share,
  hybrid_votepct = senator_year_data$votepct,
  hybrid_female = senator_year_data$female,
  hybrid_latino = senator_year_data$latino,
  hybrid_afam = senator_year_data$afam,
  hybrid_south = senator_year_data$south13,
  hybrid_election = senator_year_data$up_for_reelection,
  hybrid_senior = senator_year_data$seniority,
  hybrid_retiree = senator_year_data$retiree,
  hybrid_best_committee = senator_year_data$best_committee,
  hybrid_leader = senator_year_data$leader,
  hybrid_power_com = senator_year_data$power_committee,
  hybrid_pres = senator_year_data$pres_vote_share,
  hybrid_com_chair = senator_year_data$chair)

senDT <- merge(emIRT_data, hybrid_data, by = c("icpsrLegis", "congress"))

demDT <- senDT[caucus == "Democrat", ]

par(mfrow = c(2, 2))

plot(demDT$emIRT_extreme, demDT$hybrid_extreme)
plot(demDT$emIRT_ideal, demDT$hybrid_ideal)
plot(demDT$emIRT_response_calls, demDT$hybrid_response_calls)
plot(demDT$emIRT_response_noncalls, demDT$hybrid_response_noncalls)

par(mfrow = c(1, 1))

plot(demDT$emIRT_pres, demDT$hybrid_pres)
