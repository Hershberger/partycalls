library(nnet)
library(texreg)
library(data.table)
load("inst/extdata/votedata-partycalls.rdata")
load("test_data/house_party_calls_hybrid_seed1.RData")

votedata102 <- data.table(house_party_calls$hou102$party_call_coding)
votedata102$whip_final <- votedata$whip.final[votedata$congress == 102]
votedata102$close_votes <- votedata$close.votes[votedata$congress == 102]
votedata102$cq_votes <- votedata$cq.votes[votedata$congress == 102]
votedata102$procedural <- votedata$procedural[votedata$congress == 102]

votedata102$coding <- as.factor(votedata102$coding)
votedata102$coding2 <- factor(votedata102$coding,
  levels(votedata102$coding)[c(1, 3, 2)])
levels(votedata102$coding2)

votedata103 <- data.table(house_party_calls$hou103$party_call_coding)
votedata103$whip_final <- votedata$whip.final[votedata$congress == 103]
votedata103$close_votes <- votedata$close.votes[votedata$congress == 103]
votedata103$cq_votes <- votedata$cq.votes[votedata$congress == 103]
votedata103$procedural <- votedata$procedural[votedata$congress == 103]

votedata103$coding <- as.factor(votedata103$coding)
votedata103$coding2 <- factor(votedata103$coding,
  levels(votedata103$coding)[c(1, 3, 2)])
levels(votedata103$coding2)

votedata109 <- data.table(house_party_calls$hou109$party_call_coding)
votedata109$whip_final <- votedata$whip.final[votedata$congress == 109]
votedata109$close_votes <- votedata$close.votes[votedata$congress == 109]
votedata109$cq_votes <- votedata$cq.votes[votedata$congress == 109]
votedata109$procedural <- votedata$procedural[votedata$congress == 109]

votedata109$coding <- as.factor(votedata109$coding)
votedata109$coding2 <- factor(votedata109$coding,
  levels(votedata109$coding)[c(1, 3, 2)])
levels(votedata109$coding2)

flipflop_gray_model_0 <- multinom(coding ~ procedural + cq_votes + close_votes
  + whip_final,
  data = votedata102)
screenreg(flipflop_gray_model_0)

flipflop_gray_model_1 <- multinom(coding ~ procedural + cq_votes + close_votes
  + whip_final,
  data = votedata103)
screenreg(flipflop_gray_model_1)

flipflop_gray_model_2 <- multinom(coding ~ procedural + cq_votes + close_votes
  + whip_final,
  data = votedata109)
screenreg(flipflop_gray_model_2)

texreg(flipflop_gray_model_0)
texreg(flipflop_gray_model_1)
texreg(flipflop_gray_model_2)







load("test_data/house_party_calls_flipflop.RData")

votedata102 <- data.table(house_party_calls$hou102$party_call_coding)
votedata102$whip_final <- votedata$whip.final[votedata$congress == 102]
votedata102$close_votes <- votedata$close.votes[votedata$congress == 102]
votedata102$cq_votes <- votedata$cq.votes[votedata$congress == 102]
votedata102$procedural <- votedata$procedural[votedata$congress == 102]

votedata102$coding <- as.factor(votedata102$coding)
votedata102$coding2 <- factor(votedata102$coding,
  levels(votedata102$coding)[c(1, 3, 2)])
levels(votedata102$coding2)

votedata103 <- data.table(house_party_calls$hou103$party_call_coding)
votedata103$whip_final <- votedata$whip.final[votedata$congress == 103]
votedata103$close_votes <- votedata$close.votes[votedata$congress == 103]
votedata103$cq_votes <- votedata$cq.votes[votedata$congress == 103]
votedata103$procedural <- votedata$procedural[votedata$congress == 103]

votedata103$coding <- as.factor(votedata103$coding)
votedata103$coding2 <- factor(votedata103$coding,
  levels(votedata103$coding)[c(1, 3, 2)])
levels(votedata103$coding2)

votedata109 <- data.table(house_party_calls$hou109$party_call_coding)
votedata109$whip_final <- votedata$whip.final[votedata$congress == 109]
votedata109$close_votes <- votedata$close.votes[votedata$congress == 109]
votedata109$cq_votes <- votedata$cq.votes[votedata$congress == 109]
votedata109$procedural <- votedata$procedural[votedata$congress == 109]

votedata109$coding <- as.factor(votedata109$coding)
votedata109$coding2 <- factor(votedata109$coding,
  levels(votedata109$coding)[c(1, 3, 2)])
levels(votedata109$coding2)

flipflop_gray_model_3 <- multinom(coding ~ procedural + cq_votes + close_votes
  + whip_final,
  data = votedata102)
screenreg(flipflop_gray_model_3)

flipflop_gray_model_4 <- multinom(coding ~ procedural + cq_votes + close_votes
  + whip_final,
  data = votedata103)
screenreg(flipflop_gray_model_4)

flipflop_gray_model_5 <- multinom(coding ~ procedural + cq_votes + close_votes
  + whip_final,
  data = votedata109)
screenreg(flipflop_gray_model_5)

texreg(flipflop_gray_model_3)
texreg(flipflop_gray_model_4)
texreg(flipflop_gray_model_5)
