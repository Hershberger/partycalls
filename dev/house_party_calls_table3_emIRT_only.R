# produce replication of Table 3 relationships MV13

library(partycalls)
options(stringsAsFactors = FALSE)

# load data for analysis
load("test_data/new_whoheeds13_emIRT_only.RData")
whoheeds13 <- readstata13::read.dta13(
  "inst/extdata/who-heeds-replication-archive.dta")
setDT(whoheeds13)
whoheeds13 <- whoheeds13[, .(
  congress, icpsr, maj, retiree, bestgrosswart
)]

setnames(whoheeds13, "icpsr", "icpsrLegis")
best_committee_old <- whoheeds13[, .(congress, icpsrLegis)]

bestgrosswart_dt <- fread("inst/extdata/house_assignments_103-114-1.csv")
# committee_values <- fread("inst/extdata/committee_values.csv")
setnames(bestgrosswart_dt, "Congress", "congress")
setnames(bestgrosswart_dt, "ID #", "icpsrLegis")
setnames(bestgrosswart_dt, "Committee code", "committee_code")
bestgrosswart_dt <- bestgrosswart_dt[, .(congress, committee_code, icpsrLegis)]
committee_values <- committee_values[, ]

new_whoheeds13 <- merge(new_whoheeds13, whoheeds13, by = c("congress", "icpsr"))



f_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_votepct + south + votepct + female + afam + latino +
  seniority + freshman + retiree + bestgrosswart + leader +
  power + chair

texreg::screenreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0]),
  lm(f_extremism, new_whoheeds13[maj == 1]),
  lm(f_extremism, new_whoheeds13[maj == 0])
))

texreg::texreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1]),
  lm(f_extremism, new_whoheeds13[dem == 0]),
  lm(f_extremism, new_whoheeds13[maj == 1]),
  lm(f_extremism, new_whoheeds13[maj == 0])
))

# make replication of who heeds 2013 table 3
texreg::screenreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 107])
))

texreg::texreg(list(
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 1 & congress == 107]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 97]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 102]),
  lm(f_extremism, new_whoheeds13[dem == 0 & congress == 107])
))
