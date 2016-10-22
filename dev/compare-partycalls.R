library(data.table)

options(stringsAsFactors = FALSE)

load("inst/extdata/votedata-partycalls.RData")
load("inst/extdata/house_party_calls.RData")

votedata$partycall_binary <- 0
votedata$partycall_binary[votedata$partycall == TRUE] <- 1
setDT(votedata)
votedata[, c("voteno", "whip.final", "v1", "v2", "v3", "procedural",
  "cq.votes", "close.votes", "partycall") := NULL]
votedata[, partycall_count_old := sum(partycall_binary), by = congress]

compare_partycalls <- data.frame(congress = c(93:110))
compare_partycalls$partycall_count_new <- NA
compare_partycalls$partycall_count_new[compare_partycalls$congress == 93] <-
  length(house_party_calls$hou93$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 94] <-
  length(house_party_calls$hou94$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 95] <-
  length(house_party_calls$hou95$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 96] <-
  length(house_party_calls$hou96$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 97] <-
  length(house_party_calls$hou97$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 98] <-
  length(house_party_calls$hou98$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 99] <-
  length(house_party_calls$hou99$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 100] <-
  length(house_party_calls$hou100$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 101] <-
  length(house_party_calls$hou101$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 102] <-
  length(house_party_calls$hou102$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 103] <-
  length(house_party_calls$hou103$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 104] <-
  length(house_party_calls$hou104$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 105] <-
  length(house_party_calls$hou105$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 106] <-
  length(house_party_calls$hou106$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 107] <-
  length(house_party_calls$hou107$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 108] <-
  length(house_party_calls$hou108$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 109] <-
  length(house_party_calls$hou109$party_calls)
compare_partycalls$partycall_count_new[compare_partycalls$congress == 110] <-
  length(house_party_calls$hou110$party_calls)
setDT(compare_partycalls)

votedata[, partycall_binary := NULL]

compare_partycalls <- merge(compare_partycalls, votedata, by = "congress")

par(mfrow=c(1, 2))
plot(compare_partycalls$congress, compare_partycalls$partycall_count_new,
  type = "l", ylim = range(250:1350),
  xlab = "Congress", ylab = "Number Party Calls",
  main = "New Party Call Counts")
plot(compare_partycalls$congress, compare_partycalls$partycall_count_old,
  type = "l", ylim = range(250:1350),
  xlab = "Congress", ylab = "Number Party Calls",
  main = "Old Party Call Counts")
par(mfrow=c(1, 1))
