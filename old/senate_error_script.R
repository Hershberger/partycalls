library(data.table)
library(partycalls)

# throws error with any functions we have made so far so i'm just loading the
# actual package for this

# I fixed it within this version of the function
source("dev/functions-improvement-attempt-6.R")
load("inst/extdata/senate93-112.RData")

senate_party_calls <- code_party_calls(sen95, type = "brglm",
  sim_annealing = TRUE)
