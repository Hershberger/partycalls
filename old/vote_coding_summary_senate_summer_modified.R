library(partycalls)
library(xtable)

load("test_data/senate_party_calls_summer_modified.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

# table for all congresses
senate_coding_record <- data.table(congress = 93:112)
senate_coding_record[, party_call_count := sapply(93:112, function(x)
  length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, noncall_count := sapply(93:112, function(x)
  length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, gray_vote_count := sapply(93:112, function(x)
  length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))]

xtable(senate_coding_record)

# summary stats
sum(senate_coding_record$party_call_count)
sum(senate_coding_record$noncall_count)
sum(senate_coding_record$gray_vote_count)

mean(senate_coding_record$party_call_count)
mean(senate_coding_record$noncall_count)
mean(senate_coding_record$gray_vote_count)

sd(senate_coding_record$party_call_count)
sd(senate_coding_record$noncall_count)
sd(senate_coding_record$gray_vote_count)

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
  lopside_DT[, passed := 0]
  lopside_DT[yea_perc > 0.5, passed := 1]

  lopside_DT[, congress := congress_number]
  setnames(lopside_DT, "vt", "voteno")
  lopside_DT
}

sen_lop_coding <- lapply(93:112, get_vote_data, chamber = "senate")
sen_lop_coding <- rbindlist(sen_lop_coding)
setDT(sen_lop_coding)

sen_coding <- list()
for (i in 93:112) {
  rc <- paste0("sen", i)
  sen_coding[[rc]] <- get_party_call_coding(senate_party_calls[[rc]],
    n_iterations = 5)
  sen_coding[[rc]]$congress <- i
}
sen_coding <- rbindlist(sen_coding)
setDT(sen_coding)

sen_lop_coding <- merge(sen_lop_coding, sen_coding, by = c("congress", "voteno"))


# get summary stats for lopsided votes
lop_table <- table(sen_lop_coding$lopsided, sen_lop_coding$coding)
lop_table








