library(partycalls)

# Load data
load("test_data/new_whoheeds13_lm.RData")
house_data <- new_whoheeds13[drop == 0, ]
house_data[is.na(vote_share), vote_share := 100]
setnames(house_data,
  c("bestgrosswart", "power"),
  c("best_committee", "power_committee"))
old_names <- c("pirate100", "pfrate100", "afam")
new_names <- c("responsiveness_to_party_calls", "baseline_rate",
  "african_american")
setnames(house_data, old_names, new_names)

load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[, dem := NA_real_]
senate_data[caucus == "Democrat", dem := 1]
senate_data[caucus == "Republican", dem := 0]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]
setnames(senate_data,
  c("maj"),
  c("majority"))
setnames(senate_data, old_names, new_names)

# analysis

formula1 <- responsiveness_to_party_calls ~
  ideological_extremism + baseline_rate +
  vote_share + pres_vote_share + leader +  chair + power_committee +
  best_committee + female + african_american + latino + south +
  seniority + freshman
formula2 <- update.formula(formula1, . ~ . + up_for_reelection)

# tab-responsiveness-regressions
models <- list(
  lm(formula1, house_data),
  lm(formula1, senate_data),
  lm(formula2, senate_data))
ses <- list(
  robust_se(models[[1]],
    house_data[, congress],
    house_data[, icpsrLegis]),
  robust_se(models[[2]],
    senate_data[, congress],
    senate_data[, icpsrLegis]),
  robust_se(models[[3]],
    senate_data[, congress],
    senate_data[, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2, 3, 16, 4:15, 1))

# tab-house-models
models <- list(
  lm(formula1, house_data),
  lm(formula1, house_data[dem == 1]),
  lm(formula1, house_data[dem == 0]),
  lm(formula1, house_data[majority == 1]),
  lm(update.formula(formula1, . ~ . - chair), house_data[majority == 0]))
ses <- list(
  robust_se(models[[1]],
    house_data[, congress],
    house_data[, icpsrLegis]),
  robust_se(models[[2]],
    house_data[dem == 1, congress],
    house_data[dem == 1, icpsrLegis]),
  robust_se(models[[3]],
    house_data[dem == 0, congress],
    house_data[dem == 0, icpsrLegis]),
  robust_se(models[[4]],
    house_data[majority == 1, congress],
    house_data[majority == 1, icpsrLegis]),
  robust_se(models[[5]],
    house_data[majority == 0, congress],
    house_data[majority == 0, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2:15, 1))

# tab-senate-models
models <- list(
  lm(formula2, senate_data),
  lm(formula2, senate_data[dem == 1]),
  lm(formula2, senate_data[dem == 0]),
  lm(formula2, senate_data[majority == 1]),
  lm(update.formula(formula2, . ~ . - chair), senate_data[majority == 0]))
ses <- list(
  robust_se(models[[1]],
    senate_data[, congress],
    senate_data[, icpsrLegis]),
  robust_se(models[[2]],
    senate_data[dem == 1, congress],
    senate_data[dem == 1, icpsrLegis]),
  robust_se(models[[3]],
    senate_data[dem == 0, congress],
    senate_data[dem == 0, icpsrLegis]),
  robust_se(models[[4]],
    senate_data[majority == 1, congress],
    senate_data[majority == 1, icpsrLegis]),
  robust_se(models[[5]],
    senate_data[majority == 0, congress],
    senate_data[majority == 0, icpsrLegis]))
pvals <- mapply(function(x, y) 2 * (1 - pnorm(abs(coef(x) / y))), models, ses,
  SIMPLIFY = FALSE)
texreg::texreg(models, override.se = ses, override.pvalues = pvals,
  custom.coef.names = fix_coef_names(models),
  reorder.coef = c(2:3, 16, 4:15, 1))
