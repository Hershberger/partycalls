library(data.table)
library(readstata13)
library(bigKRLS)
lep <- read.dta13("inst/extdata/LEP93to113.dta")
setDT(lep)
lep[, presvote := 100 - dpres_pct]
lep[dem == 1, presvote := dpres_pct]
lep_selection <- lep[, .(
  les,
  leslag,
  dwnom1, dwnom2, meddist, majdist,
  presvote, votepct,
  state_leg, slprofess,
  afam, latino, female,
  dem, majority,
  chairall, subchr, power, budget, leader,
  seniority, freshman, south
)]
lep_selection <- na.omit(lep_selection)
X <- copy(lep_selection)
X[, les := NULL]
ok <- sample(nrow(X), 1000)
X_sample <- data.matrix(X[ok])
y_sample <- data.matrix(lep_selection[ok, les])

model <- bigKRLS(X = X_sample, y = y_sample, noisy = TRUE)
