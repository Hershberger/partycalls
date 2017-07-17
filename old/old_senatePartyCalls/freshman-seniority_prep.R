library(data.table)

load("inst/extdata/senator_year_data.Rdata")
load("inst/extdata/senator_data.Rdata")
freshman_DT <- setDT(read.csv("inst/extdata/freshman_DT.csv"))
seniority_DT <- setDT(read.csv("inst/extdata/seniority_DT.csv"))

# getting freshman year/session data associated with mc

freshman_DT <- merge(freshman_DT, senator_data,
  by = "icpsrLegis")


# getting seniority associated with mc

seniority_DT <- merge(seniority_DT, senator_year_data,
  by = c("icpsrLegis", "congress"))