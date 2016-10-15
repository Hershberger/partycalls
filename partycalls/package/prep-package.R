# build package
devtools::document()
devtools::build()
devtools::install()

# read in data
options(stringsAsFactors = FALSE)
library(partycalls)
load("inst/extdata/houKHfiles001-111.rdata")
load("inst/extdata/senate93-112.RData")
