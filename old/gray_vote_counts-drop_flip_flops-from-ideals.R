library(partycalls)
library(data.table)

load("test_data/house_party_calls_drop_flip_flop_from_ideal.RData")

new_partycalls <- rbindlist(lapply(house_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

new_partycalls$gray <- 0
new_partycalls$gray[new_partycalls$new_coding == "gray"] <- 1
new_partycalls[, mean_gray := mean(gray), by = congress]
summary_DT <- unique(new_partycalls[, list(congress, mean_gray)])
summary_DT
summary(summary_DT$mean_gray)
sd(summary_DT$mean_gray)

# pdf(file="plots/gray_vote_percents_drop_flip_flops_from_ideal.pdf",
#   width=8, height = 8, family="Times")
graphics::plot(new_partycalls$congress, new_partycalls$mean_gray, type = "b", xlim = c(93,109), ylim = c(0,0.30),
  xlab = "House Session", ylab = "Percent Gray Votes")
# dev.off()

load("inst/extdata/votedata-partycalls.RData")
setDT(votedata)
old_partycalls <- votedata[congress < 110, .(congress, voteno, partycall)]
old_partycalls[, old_coding := "gray"]
old_partycalls[partycall == TRUE, old_coding := "party call"]
old_partycalls[partycall == FALSE, old_coding := "noncall"]
old_partycalls[, partycall := NULL]
old_partycalls[, congress := as.character(congress)]
old_partycalls[, voteno := as.character(voteno)]

X <- merge(old_partycalls, new_partycalls, by = c("congress", "voteno"))
table_x <- X[, table(old_coding, new_coding)]
table_x
chisq.test(table_x)

X103 <- subset(X, congress %in% 103)
X103[, table(old_coding, new_coding)]

X102 <- subset(X, congress %in% 102)
X102[, table(old_coding, new_coding)]

X108 <- subset(X, congress %in% 108)
X108[, table(old_coding, new_coding)]

X109 <- subset(X, congress %in% 109)
X109[, table(old_coding, new_coding)]

X106 <- subset(X, congress %in% 106)
X106[, table(old_coding, new_coding)]
