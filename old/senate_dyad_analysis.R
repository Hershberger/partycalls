library(partycalls)
library(xtable)
set.seed(49243984)
load("test_data/senate_data_lm.RData")

# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  tr = up_for_reelection, y = pirate100 - pfrate100)]
setorder(DATA, stabb, congress, class)

# make dems majority for congress 107
DATA[congress == 107 & caucus == "Democrat", maj := 1]
DATA[congress == 107 & caucus == "Republican", maj := 0]

# subset to cases with two senators, one treated, one control
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, mean_tr := mean(tr), .(stabb, congress)]

dyad_dat <- merge(DATA[tr == 1, ], DATA[mean_tr == 0.5 & tr == 0, ],
  by = c("stabb", "congress"))

dyad_dat[, y := y.x - y.y]
dyad_dat[, both_reps := (caucus.x == "Republican") * (caucus.y == "Republican")]
dyad_dat[, both_dems := (caucus.x == "Democrat") * (caucus.y == "Democrat")]
dyad_dat[, split_rep := (caucus.x == "Republican") * (caucus.y == "Democrat")]
dyad_dat[, split_dem := (caucus.x == "Democrat") * (caucus.y == "Republican")]
dyad_dat[, split_party := split_rep + split_dem]
dyad_dat[, tr_maj := maj.x]

dyad_1 <- lfe::felm(y ~ tr_maj * split_rep + tr_maj *
    split_dem | stabb + congress | 0 | stabb + congress, dyad_dat)
dyad_2 <- lfe::felm(y ~ tr_maj * split_party + tr_maj * both_reps | stabb +
    congress | 0 | stabb + congress, dyad_dat)
dyad_3 <- lfe::felm(y ~ tr_maj * split_party + tr_maj * both_dems | stabb +
    congress | 0 | stabb + congress, dyad_dat)
dyad_4 <- lfe::felm(y ~ tr_maj * both_reps + tr_maj * both_dems | stabb +
    congress | 0 | stabb + congress, dyad_dat)

dyad_1_table <- xtable(dyad_1, auto = TRUE,
  caption = "Split-Party State Effects, Difference-in-Differences")
print(dyad_1_table, table.placement = "H", caption.placement = "top")

dyad_2_table <- xtable(dyad_2, auto = TRUE,
  caption = "Seat Type Effects, Split and 2 Republicans, Difference-in-Differences")
print(dyad_2_table, table.placement = "H", caption.placement = "top")

dyad_3_table <- xtable(dyad_3, auto = TRUE,
  caption = "Seat Type Effects, Split and 2 Democrats, Difference-in-Differences")
print(dyad_3_table, table.placement = "H", caption.placement = "top")

dyad_4_table <- xtable(dyad_4, auto = TRUE,
  caption = "Seat Type Effects, 2 Republicans and 2 Democrats, Difference-in-Differences")
print(dyad_4_table, table.placement = "H", caption.placement = "top")

