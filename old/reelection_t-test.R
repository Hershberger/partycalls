library(partycalls)
library(xtable)

load("test_data/senate_data_lm.RData")

senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]



DATA <- merge(senate_data, senate_data[, .N,.(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2, ]

DATA_0 <- DATA[up_for_reelection == 0, ]
DATA_1 <- DATA[up_for_reelection == 1, ]

tests <- list()

tests[[1]] <- t.test(DATA_0[, maj], DATA_1[, maj], na.rm = TRUE)
tests[[2]] <- t.test(DATA_0[, pres_vote_share], DATA_1[, pres_vote_share],
  na.rm = TRUE)
tests[[3]] <- t.test(DATA_0[, vote_share], DATA_1[, vote_share], na.rm = TRUE)
tests[[4]] <- t.test(DATA_0[, south], DATA_1[, south], na.rm = TRUE)
tests[[5]] <- t.test(DATA_0[, first_term], DATA_1[, first_term], na.rm = TRUE)
tests[[6]] <- t.test(DATA_0[, leader], DATA_1[, leader], na.rm = TRUE)
tests[[7]] <- t.test(DATA_0[, chair], DATA_1[, chair], na.rm = TRUE)
tests[[8]] <- t.test(DATA_0[, best_committee], DATA_1[, best_committee],
  na.rm = TRUE)
tests[[9]] <- t.test(DATA_0[, power_committee], DATA_1[, power_committee],
  na.rm = TRUE)
tests[[10]] <- t.test(DATA_0[, seniority], DATA_1[, seniority], na.rm = TRUE)
tests[[11]] <- t.test(DATA_0[, female], DATA_1[, female], na.rm = TRUE)
tests[[12]] <- t.test(DATA_0[, afam], DATA_1[, afam], na.rm = TRUE)
tests[[13]] <- t.test(DATA_0[, latino], DATA_1[, latino], na.rm = TRUE)
tests[[14]] <- t.test(DATA_0[, gingrich_senator], DATA_1[, gingrich_senator],
  na.rm = TRUE)
tests[[15]] <- t.test(DATA_0[, ideological_extremism],
  DATA_1[, ideological_extremism],
  na.rm = TRUE)
tests[[16]] <- t.test(DATA_0[, votes], DATA_1[, votes],
  na.rm = TRUE)

results <- as.data.frame(sapply(tests, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
}))

vars <- c("maj", "pres_vote_share", "vote_share", "south", "first_term",
  "leader", "chair", "best_committee", "power_committee", "seniority",
  "female", "afam", "latino", "gingrich_senator", "ideological_extremism",
  "votes")

rownames(results) <- c("Mean, Not Reelection",
  "Mean, Reelection", "Lower Bound", "Upper Bound", "p-value")
colnames(results) <- vars

results <- t(results)

reelection_tex <- xtable(results, auto = TRUE,
  caption = "T-Test, Up for Reelection")
digits(reelection_tex) <- 4
print(reelection_tex, include.rownames = TRUE, placement = "H",
  caption.placement = "top")

