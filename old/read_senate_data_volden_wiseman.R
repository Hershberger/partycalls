library(xlsx)
library(data.table)

# NOTE: read.xlsx2 is way faster than read.xlsx
legislative_effectiveness <-
  read.xlsx2("inst/extdata/93_113_senate_variables.xlsx", sheetIndex = 1)
setDT(legislative_effectiveness)
legislative_effectiveness <- legislative_effectiveness[-1, ]


