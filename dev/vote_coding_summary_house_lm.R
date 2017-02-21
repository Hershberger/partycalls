library(partycalls)
library(xtable)

load("test_data/house_party_calls_lm.RData")
names(house_party_calls) <- paste0("hou", 93:112)

# table for all congresses
house_coding_record <- data.frame(row.names = 93:112)
house_coding_record$party_call_count <- sapply(93:112, function(x)
  length(get_party_calls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$noncall_count <- sapply(93:112, function(x)
  length(get_noncalls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$gray_vote_count <- sapply(93:112, function(x)
  length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))

print(xtable(house_coding_record), include.rownames = TRUE)

# summary stats
sum(house_coding_record$party_call_count)
sum(house_coding_record$noncall_count)
sum(house_coding_record$gray_vote_count)

mean(house_coding_record$party_call_count)
mean(house_coding_record$noncall_count)
mean(house_coding_record$gray_vote_count)

sd(house_coding_record$party_call_count)
sd(house_coding_record$noncall_count)
sd(house_coding_record$gray_vote_count)

# for more in depth coding info
get_vote_data <- function(chamber, congress_number) {
  if (chamber == "house") {
    rc <- get(paste0("h", sprintf("%03.f", congress_number)))
    party_calls <- house_party_calls
  } else if (chamber == "senate") {
    rc <- get(paste0("sen", congress_number))
    party_calls <- senate_party_calls
  } else {
    stop("pick a chamber")
  }
  rc <- emIRT::convertRC(rc, type = "binIRT")
  vote_data <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
  vote_data$y <- as.vector(rc$votes)
  vote_data$party <- rc$legis.data$party
  vote_data[y %in% c(0, 9), y:= NA]
  vote_data[y == -1, y:= 0]

  lopside_DT <- vote_data[, .(yea_perc = mean(y, na.rm = TRUE)), by = vt]
  lopside_DT[, lopsided := 0]
  lopside_DT[yea_perc >= 0.65 | yea_perc <= 0.35, lopsided := 1]
  lopside_DT[, congress := congress_number]
  setnames(lopside_DT, "vt", "voteno")
  lopside_DT
}

hou_lop_coding <- lapply(93:112, get_vote_data, chamber = "house")
hou_lop_coding <- rbindlist(hou_lop_coding)
setDT(hou_lop_coding)

hou_coding <- list()
for (i in 93:112) {
  rc <- paste0("hou", i)
  hou_coding[[rc]] <- get_party_call_coding(house_party_calls[[rc]],
    n_iterations = 5)
  hou_coding[[rc]]$congress <- i
}
hou_coding <- rbindlist(hou_coding)
setDT(hou_coding)

hou_lop_coding <- merge(hou_lop_coding, hou_coding, by = c("congress", "voteno"))

hou_lop_coding[, lop_close := "close"]
hou_lop_coding[lopsided == 1, lop_close := "lopsided"]

level_calls <- c("party call", "noncall", "gray")
level_lop <- c("lopsided", "close")

hou_lop_coding[, lopsided := factor(lop_close, levels = level_lop)]
hou_lop_coding[, coding := factor(coding, levels = level_calls)]


# get summary stats for lopsided votes
lopside_table <- table(hou_lop_coding$lopsided, hou_lop_coding$coding)
xtable(lopside_table)
