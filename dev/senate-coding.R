library(partycalls)

# throws error with any functions we have made so far so i'm just loading the
# actual package for this

# I fixed it within this version of the function
source("dev/functions-improvement-attempt-6.R")
load("inst/extdata/senate93-112.RData")

senate_party_calls <- lapply(93:112, code_party_calls_by_congress_number,
  chamber = "senate", sim_annealing = TRUE)
