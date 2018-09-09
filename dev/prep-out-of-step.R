# invisible(file.remove(list.files("data/", full.names = TRUE)))
# devtools::document()
# devtools::build()
# devtools::install()
library(bigKRLS)
library(ggplot2)
# MAKE DATA TO STORE IN PACKAGE HERE
setwd("~/campaignfinance")
source("package/build-house-data-1980-2000-w-indexp.R")
source("package/build-house-data-2002-2016-w-indexp.R")
# make office datasets ----
setcolorder(house_data_pre_2002, names(house_data_post_2002))
house_data <- rbind(house_data_pre_2002, house_data_post_2002)
house_data[, dem_expenditure_advantage :=
    (dem_expenditure - rep_expenditure) / 1e6]
house_data[, dem_presvote_advantage := (dpres - 50)]
house_data[,
  relative_rep_extremism := (abs(rep_cf) - abs(dem_cf)) / 1e1]

# make candidates datasets ----
house_candidates <- rbind(
  house_data[, .(year, district,
    candid = dem_candid, fecid = dem_fecid,
    candidate_name = dem_candidate_name,
    party = "D")],
  house_data[, .(year, district,
    candid = rep_candid, fecid = rep_fecid,
    candidate_name = rep_candidate_name,
    party = "R")])
house_candidates[candidate_name == "", candidate_name := NA]
# house_candidates <- house_candidates[!is.na(district) & !is.na(candidate_name)]

# make outside money ----
source("package/outside-money.R")

house_data <- merge(house_data,
  outside_money[, .(
    year, rep_candid = trimws(candidate_id),
    rep_support_electioneering_total =
      support_electioneering_total,
    rep_oppose_electioneering_total =
      oppose_electioneering_total,
    rep_support_independent_expenditure_total =
      support_independent_expenditure_total,
    rep_oppose_independent_expenditure_total =
      oppose_independent_expenditure_total,
    rep_support_comm_cost_total =
      support_comm_cost_total,
    rep_oppose_comm_cost_total =
      oppose_comm_cost_total,
    rep_party_coord_total =
      party_coord_total
  )],
  by = c("year", "rep_candid"),
  all.x = TRUE)

house_data <- merge(house_data,
  outside_money[, .(
    year, dem_candid = trimws(candidate_id),
    dem_support_electioneering_total =
      support_electioneering_total,
    dem_oppose_electioneering_total =
      oppose_electioneering_total,
    dem_support_independent_expenditure_total =
      support_independent_expenditure_total,
    dem_oppose_independent_expenditure_total =
      oppose_independent_expenditure_total,
    dem_support_comm_cost_total =
      support_comm_cost_total,
    dem_oppose_comm_cost_total =
      oppose_comm_cost_total,
    dem_party_coord_total =
      party_coord_total
  )],
  by = c("year", "dem_candid"),
  all.x = TRUE)

house_data[is.na(rep_support_electioneering_total),
  rep_support_electioneering_total := 0]
house_data[is.na(rep_oppose_electioneering_total),
  rep_oppose_electioneering_total := 0]
house_data[is.na(rep_support_independent_expenditure_total),
  rep_support_independent_expenditure_total := 0]
house_data[is.na(rep_oppose_independent_expenditure_total),
  rep_oppose_independent_expenditure_total := 0]
house_data[is.na(rep_support_comm_cost_total),
  rep_support_comm_cost_total := 0]
house_data[is.na(rep_oppose_comm_cost_total),
  rep_oppose_comm_cost_total := 0]
house_data[is.na(rep_party_coord_total),
  rep_party_coord_total := 0]

house_data[is.na(dem_support_electioneering_total),
  dem_support_electioneering_total := 0]
house_data[is.na(dem_oppose_electioneering_total),
  dem_oppose_electioneering_total := 0]
house_data[is.na(dem_support_independent_expenditure_total),
  dem_support_independent_expenditure_total := 0]
house_data[is.na(dem_oppose_independent_expenditure_total),
  dem_oppose_independent_expenditure_total := 0]
house_data[is.na(dem_support_comm_cost_total),
  dem_support_comm_cost_total := 0]
house_data[is.na(dem_oppose_comm_cost_total),
  dem_oppose_comm_cost_total := 0]
house_data[is.na(dem_party_coord_total),
  dem_party_coord_total := 0]

house_data[,
  dem_expenditure_w_outside := dem_expenditure +
    dem_support_electioneering_total +
    rep_oppose_electioneering_total +
    dem_support_independent_expenditure_total +
    rep_oppose_independent_expenditure_total +
    dem_support_comm_cost_total +
    rep_oppose_comm_cost_total +
    dem_party_coord_total]

house_data[,
  rep_expenditure_w_outside := rep_expenditure +
    rep_support_electioneering_total +
    dem_oppose_electioneering_total +
    rep_support_independent_expenditure_total +
    dem_oppose_independent_expenditure_total +
    rep_support_comm_cost_total +
    dem_oppose_comm_cost_total +
    rep_party_coord_total]

house_data[, .N, .(is.na(dem_expenditure), is.na(dem_expenditure_w_outside))]
house_data[, .N, .(is.na(rep_expenditure), is.na(rep_expenditure_w_outside))]

# make spending advantages and total spending ----

house_data[,
  dem_expenditure_advantage :=
    (dem_expenditure - rep_expenditure) / 1e6]
house_data[,
  dem_expenditure_advantage_w_outside :=
    (dem_expenditure_w_outside - rep_expenditure_w_outside) / 1e6]
house_data[, total_spending := dem_expenditure + rep_expenditure]
house_data[, total_spending_w_outside :=
    dem_expenditure_w_outside + rep_expenditure_w_outside]

# make analysis datasets ----

house_data <- house_data[,
  .(year, district, dem_candid, rep_candid, state, cd,
    dem_candidate_name, rep_candidate_name,
    dem_vote, rep_vote,
    dem_expenditure_advantage,
    dem_expenditure_advantage_w_outside,
    dem_expenditure, rep_expenditure,
    dem_expenditure_w_outside, rep_expenditure_w_outside,
    total_spending, total_spending_w_outside,
    dem_incumbent, quality_challenger,
    relative_rep_extremism,
    dem_cfscore = dem_cf, rep_cfscore = rep_cf,
    dem_presvote_advantage,
    contest_type = as.numeric(contest_type), special,
    inc, redist, unopposed, thirdother)]

conv_factors <- data.table(year = 1980:2018,
  year_scale = as.numeric(scan("dev/cpi.txt", what = "raw")))
house_data <- merge(house_data, conv_factors, by = "year")
house_data[,
  real_dem_expenditure_advantage := dem_expenditure_advantage / year_scale]
house_data[,
  real_dem_expenditure_advantage_w_outside :=
    dem_expenditure_advantage_w_outside / year_scale]

#------------------------------------------------------------------------------#

nat_presvote <- fread("inst/extdata/national-pres-vote.csv")
house_data[, year_pres_vote := year - (year %% 4)]
house_data <- merge(house_data, nat_presvote, by.x = "year_pres_vote",
  by.y = "year")
house_data[, adj_dem_presvote_advantage :=
    dem_presvote_advantage -
    100 * (dem_nat_pres_vote / (dem_nat_pres_vote + rep_nat_pres_vote))]

#------------------------------------------------------------------------------#

save(house_data, file = "~/desktop/house_data.RData")

party_calls_data <- copy(partycalls::house_data)
party_calls_data[, district := paste0(stabb, sprintf("%02i", cd))]
party_calls_data[, next_election_year := congress * 2 + 1788]


x <- merge(
  house_data,
  party_calls_data[, .(next_election_year, district,
    congress, icpsrLegis, thomas_name, dem,
    responsiveness_to_party_calls, baseline_rate, ideological_extremism)],
  by.x = c("year", "district"), by.y = c("next_election_year", "district"),
  all.x = TRUE
)
xx <- rbind(
  x[dem == 1 & contest_type %in% 1:2],
  x[dem == 0 & contest_type %in% 3:4]
)

house_X_dem <- data.matrix(xx[
  !is.na(dem_expenditure_advantage) &
    !is.na(relative_rep_extremism) &
    !is.na(quality_challenger) &
    !is.na(contest_type) &
    special == 0 &
    dem == 1, .(
      responsiveness_to_party_calls, baseline_rate,
      real_dem_expenditure_advantage, dem_presvote_advantage,
      relative_rep_extremism,
      # dem_inc_lq_chal = as.numeric(contest_type == 1),
      dem_inc_hq_chal = as.numeric(contest_type == 2),
      # rep_inc_lq_chal = as.numeric(contest_type == 3),
      # rep_inc_hq_chal = as.numeric(contest_type == 4),
      # open_both_hi = as.numeric(contest_type == 5),
      # open_hi_dem_lo_rep = as.numeric(contest_type == 6),
      # open_lo_dem_hi_rep = as.numeric(contest_type == 7),
      y80 = as.numeric(year == 1980), y82 = as.numeric(year == 1982),
      y84 = as.numeric(year == 1984), y86 = as.numeric(year == 1986),
      y88 = as.numeric(year == 1988), y90 = as.numeric(year == 1990),
      y92 = as.numeric(year == 1992), y94 = as.numeric(year == 1994),
      y96 = as.numeric(year == 1996), y98 = as.numeric(year == 1998),
      y00 = as.numeric(year == 2000), y02 = as.numeric(year == 2002),
      y04 = as.numeric(year == 2004), y06 = as.numeric(year == 2006),
      y08 = as.numeric(year == 2008), y10 = as.numeric(year == 2010),
      y12 = as.numeric(year == 2012)#,
      # y14 = as.numeric(year == 2014),
      # y16 = as.numeric(year == 2016)
      )])
house_Y_dem <- xx[
  !is.na(dem_expenditure_advantage) &
    !is.na(relative_rep_extremism) &
    !is.na(quality_challenger) &
    !is.na(contest_type) &
    special == 0 &
    dem == 1,
  100 * dem_vote / (dem_vote + rep_vote)]


house_X_rep <- data.matrix(xx[
  !is.na(dem_expenditure_advantage) &
    !is.na(relative_rep_extremism) &
    !is.na(quality_challenger) &
    !is.na(contest_type) &
    special == 0 &
    dem == 0, .(
      responsiveness_to_party_calls, baseline_rate,
      -real_dem_expenditure_advantage, 100-dem_presvote_advantage,
      -relative_rep_extremism,
      # dem_inc_lq_chal = as.numeric(contest_type == 1),
      # dem_inc_hq_chal = as.numeric(contest_type == 2),
      # rep_inc_lq_chal = as.numeric(contest_type == 3),
      rep_inc_hq_chal = as.numeric(contest_type == 4),
      # open_both_hi = as.numeric(contest_type == 5),
      # open_hi_dem_lo_rep = as.numeric(contest_type == 6),
      # open_lo_dem_hi_rep = as.numeric(contest_type == 7),
      y80 = as.numeric(year == 1980), y82 = as.numeric(year == 1982),
      y84 = as.numeric(year == 1984), y86 = as.numeric(year == 1986),
      y88 = as.numeric(year == 1988), y90 = as.numeric(year == 1990),
      y92 = as.numeric(year == 1992), y94 = as.numeric(year == 1994),
      y96 = as.numeric(year == 1996), y98 = as.numeric(year == 1998),
      y00 = as.numeric(year == 2000), y02 = as.numeric(year == 2002),
      y04 = as.numeric(year == 2004), y06 = as.numeric(year == 2006),
      y08 = as.numeric(year == 2008), y10 = as.numeric(year == 2010),
      y12 = as.numeric(year == 2012)#,
      # y14 = as.numeric(year == 2014),
      # y16 = as.numeric(year == 2016)
    )])
house_Y_rep <- xx[
  !is.na(dem_expenditure_advantage) &
    !is.na(relative_rep_extremism) &
    !is.na(quality_challenger) &
    !is.na(contest_type) &
    special == 0 &
    dem == 0,
  100-100 * dem_vote / (dem_vote + rep_vote)]

model <- bigKRLS::bigKRLS(X = rbind(house_X_dem, house_X_rep),
  y = c(house_Y_dem, house_Y_rep), Ncores = 4)
# save.bigKRLS(house_outcome_model, "house_outcome_model")
house_out <- as.data.table(cbind(rbind(house_X_dem, house_X_rep),
  model$derivatives))
varnames <- colnames(rbind(house_X_dem, house_X_rep))
setnames(house_out, c(varnames, paste0("d_", varnames)))
summary(model)

ggplot(house_out,
  aes(baseline_rate, 100*d_baseline_rate)) +
  geom_smooth()

ggplot(house_out,
  aes(responsiveness_to_party_calls, 100*d_responsiveness_to_party_calls)) +
  geom_smooth()

# xx <- xx[
#   !is.na(dem_expenditure_advantage) &
#     !is.na(relative_rep_extremism) &
#     !is.na(quality_challenger) &
#     !is.na(contest_type)]
# xx[, `:=`(
#   dem_inc_lq_chal = as.numeric(contest_type == 1),
#   dem_inc_hq_chal = as.numeric(contest_type == 2),
#   rep_inc_lq_chal = as.numeric(contest_type == 3),
#   rep_inc_hq_chal = as.numeric(contest_type == 4))]
# summary(
#   lm(100 * dem_vote / (dem_vote + rep_vote) ~
#       responsiveness_to_party_calls +
#       baseline_rate +
#       real_dem_expenditure_advantage +
#       dem_presvote_advantage +
#       relative_rep_extremism +
#       dem_inc_lq_chal +
#       dem_inc_hq_chal +
#       rep_inc_lq_chal +
#       rep_inc_hq_chal +
#       factor(year) - 1,
#     data = xx))


