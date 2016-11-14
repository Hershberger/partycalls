library(nnet)
library(data.table)
load("inst/extdata/votedata-partycalls.rdata")
load("test_data/house_party_calls_flipflop.RData")

votedata103 <- data.table(house_party_calls$hou103$party_call_coding)
votedata103$whip_final <- votedata$whip.final[votedata$congress == 103]
votedata103$close_votes <- votedata$close.votes[votedata$congress == 103]
votedata103$cq_votes <- votedata$cq.votes[votedata$congress == 103]
votedata103$procedural <- votedata$procedural[votedata$congress == 103]

votedata103$coding <- as.factor(votedata103$coding)
votedata103$coding2 <- factor(votedata103$coding,
  levels(votedata103$coding)[c(1, 3, 2)])
levels(votedata103$coding2)

flipflop_gray_model_1 <- multinom(coding ~ procedural + cq_votes
  + close_votes,
  data = votedata103)
summary(flipflop_gray_model_1)
