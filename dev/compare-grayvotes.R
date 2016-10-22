library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)

load("inst/extdata/votedata-partycalls.rdata")

gray_votes_DT <- data.table(new_gray_votes =
    c(length(unique(get_gray_votes(house_party_calls$hou93$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou94$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou95$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou96$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou97$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou98$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou99$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou100$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou101$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou102$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou103$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou104$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou105$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou106$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou107$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou108$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou109$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou110$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou111$record_of_coding))),
      length(unique(get_gray_votes(house_party_calls$hou112$record_of_coding)))))

gray_votes_DT[, congress := c(93:112)]

old_gray <- data.frame(table(votedata$congress, is.na(votedata$partycall)))
old_gray <- old_gray[19:36,]
old_gray$Var1 <- as.numeric(93:110)


par(mfrow = c(1, 2))

plot(gray_votes_DT$congress, gray_votes_DT$new_gray_votes,
  type = "l", ylim = range(0:400), xlim = range(93:112),
  xlab = "Congress", ylab = "Number Gray Votes",
  main = "New Gray Vote Counts")

plot(old_gray$Var1, old_gray$Freq,
  type = "l", xlab = "Congress", ylab = "Number Gray Votes",
  main = "Old Gray Vote Counts")

par(mfrow = c(1, 1))

