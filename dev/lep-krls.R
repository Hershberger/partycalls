library(data.table)
library(readstata13)
library(bigKRLS)
library(survey)

make_quantile <- function(x, k)
{
  levels <- seq(0, 1, length.out = k + 1)
  breaks <- quantile(x, levels)
  quantiles <- cut(x, breaks, include.lowest = TRUE)
  as.numeric(quantiles)
}

lep <- read.dta13("inst/extdata/LEP93to113.dta")
setDT(lep)
lep[, presvote := 100 - dpres_pct]
lep[dem == 1, presvote := dpres_pct]

lep_selection <- lep[!is.na(icpsr), .(
  icpsr, year, congress,
  votepct,
  benchratio,
  seniority,
  chairall, subchr,
  state_leg, slprofess, state_leg_prof,
  majority,
  maj_leader, min_leader,
  power, meddist,
  female,
  majwomen, minwomen,
  afam, latino,
  presvote
)]
lep_selection <- merge(
  lep_selection,
  lep_selection[, .(benchratiolag = benchratio,
    year = year + 2, congress = congress + 1, icpsr)],
  by = c("year", "congress", "icpsr"))
lep_selection <- na.omit(lep_selection)




vote_in_next_election <- lep_selection[, .(icpsr, year, votepct)]
behavior_in_prev_congress <- copy(lep_selection)
setnames(behavior_in_prev_congress, "votepct", "votepct_prev")
behavior_in_prev_congress[, year := year + 2]
behavior_in_prev_congress[, tr := as.numeric(benchratio > 1)]
behavior_in_prev_congress[, q_br := make_quantile(benchratio, 5)]
DATA <- merge(
  vote_in_next_election,
  behavior_in_prev_congress,
  by = c("icpsr", "year")
)
DATA[, log_benchratio := log(benchratio)]
DATA[benchratio == 0, log_benchratio := log(0.001942827)]

# DATA <- DATA[chairall == 0 & leader == 0 & subchr == 0]
# DATA[, .N, tr]
library(mgcv)

k <- 10
model <- mgcv::gam(log_benchratio ~
    s(votepct_prev) + s(seniority) + s(meddist) + s(presvote) +
    chairall + subchr +
    state_leg + slprofess + state_leg_prof +
    majority +
    maj_leader + min_leader +
    power +
    majwomen + minwomen +
    afam +  latino,
  data = DATA)
DATA[, pfunction := predict(model)]
DATA[, q_pfunction := make_quantile(pfunction, k)]
sapply(1:k, function(x)
  unname(coef(
    gam(votepct ~ log_benchratio + s(presvote),
      data = DATA[q_pfunction == x]))["log_benchratio"]))

boots <- replicate(100, {
  ok <- sample(nrow(DATA), nrow(DATA), replace = TRUE)
  boot_data <- DATA[ok]
  model <- mgcv::gam(log_benchratio ~
      s(votepct_prev) + s(seniority) + s(meddist) + s(presvote) +
      chairall + subchr +
      state_leg + slprofess + state_leg_prof +
      majority +
      maj_leader + min_leader +
      power +
      majwomen + minwomen +
      afam +  latino,
    data = boot_data)
  boot_data[, pfunction := predict(model)]
  boot_data[, q_pfunction := make_quantile(pfunction, k)]
  sapply(1:k, function(x)
    unname(coef(gam(votepct ~ log_benchratio + s(presvote),
      data = boot_data[q_pfunction == x]))["log_benchratio"]))
})
t(apply(boots, 1, function(x) c(mean(x), quantile(x, c(.025, .975)))))



k <- 10
model <- mgcv::gam(log_benchratio ~
    s(votepct_prev) + s(seniority) + s(meddist) +
    chairall + subchr +
    state_leg + slprofess + state_leg_prof +
    maj_leader + min_leader +
    power +
    majwomen + minwomen +
    afam + latino,
  data = DATA[majority == 1])
DATA[majority == 1, pfunction := predict(model)]
DATA[majority == 1, q_pfunction := make_quantile(pfunction, k)]
sapply(1:k, function(x)
  unname(coef(lm(votepct ~ log_benchratio,
    DATA[q_pfunction == x & majority == 1]))[2]))

boots <- replicate(100, {
  ok <- sample(nrow(DATA[majority == 1]), nrow(DATA[majority == 1]), replace = TRUE)
  boot_data <- DATA[majority == 1][ok]
  model <- mgcv::gam(log_benchratio ~
      s(votepct_prev) + s(seniority) + s(meddist) +
      maj_leader + min_leader + chairall + subchr + power +
      state_leg + slprofess + state_leg_prof +
      majwomen + minwomen + afam + latino,
    data = boot_data)
  boot_data[, pfunction := predict(model)]
  boot_data[, q_pfunction := make_quantile(pfunction, k)]
  sapply(1:k, function(x)
    unname(coef(lm(votepct ~ log_benchratio,
      boot_data[q_pfunction == x]))[2]))
})
t(apply(boots, 1, function(x) c(mean(x), quantile(x, c(.025, .975)))))

model <- mgcv::gam(log_benchratio ~
    s(votepct_prev) + s(seniority) + s(meddist) +
    chairall + subchr +
    state_leg + slprofess + state_leg_prof +
    maj_leader + min_leader +
    power +
    majwomen + minwomen +
    afam + latino,
  data = DATA[majority == 0])
DATA[majority == 0, pfunction := predict(model)]
DATA[majority == 0, q_pfunction := make_quantile(pfunction, k)]
sapply(1:k, function(x)
  unname(coef(lm(votepct ~ log_benchratio,
    DATA[q_pfunction == x & majority == 0]))[2]))
boots <- replicate(100, {
  ok <- sample(nrow(DATA[majority == 0]), nrow(DATA[majority == 0]),
    replace = TRUE)
  boot_data <- DATA[majority == 0][ok]
  model <- mgcv::gam(log_benchratio ~
      s(votepct_prev) + s(seniority) + s(meddist) +
      maj_leader + min_leader + chairall + subchr + power +
      state_leg + slprofess + state_leg_prof +
      majwomen + minwomen + afam + latino,
    data = boot_data)
  boot_data[, pfunction := predict(model)]
  boot_data[, q_pfunction := make_quantile(pfunction, k)]
  sapply(1:k, function(x)
    unname(coef(lm(votepct ~ log_benchratio + ,
      boot_data[q_pfunction == x]))[2]))
})
t(apply(boots, 1, function(x) c(mean(x), quantile(x, c(.025, .975)))))



# DATA[, weight := tr / pscore + (1 - tr) / (1 - pscore)]
# DATA[, weight := pmin(weight, quantile(weight, .9))]

#summary(svyglm(votepct ~ tr,
#  svydesign(~1, weights = DATA$weight, data = DATA)))



X <- copy(lep_selection)

X[, les := NULL]
ok <- sample(nrow(X), 1000)
X_sample <- data.matrix(X[ok])
y_sample <- data.matrix(lep_selection[ok, les])

model <- bigKRLS(X = X_sample, y = y_sample, noisy = TRUE)
