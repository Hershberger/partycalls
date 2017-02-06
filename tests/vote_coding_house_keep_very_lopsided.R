library(partycalls)
library(xtable)

load("test_data/house_party_calls_keep_very_lopsided.RData")
names(house_party_calls) <- paste0("hou", 93:112)

# table for all congresses
house_coding_record <- data.frame(row.names = 93:112)
house_coding_record$party_call_count <- sapply(93:112, function(x)
  length(get_party_calls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$noncall_count <- sapply(93:112, function(x)
  length(get_noncalls(house_party_calls[[paste0("hou", x)]])))
house_coding_record$gray_vote_count <- sapply(93:112, function(x)
  length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))

xtable(house_coding_record)

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

# democrat table
house_coding_record$congress <- c(93:112)
setDT(house_coding_record)
house_coding_record <- house_coding_record[, .(congress, party_call_count,
  noncall_count, gray_vote_count)]
dem_majority <- c(93:103, 110:111)
dem_house_record <- house_coding_record[congress %in% dem_majority, ]
xtable(dem_house_record)

# summary stats
sum(dem_house_record$party_call_count)
sum(dem_house_record$noncall_count)
sum(dem_house_record$gray_vote_count)

mean(dem_house_record$party_call_count)
mean(dem_house_record$noncall_count)
mean(dem_house_record$gray_vote_count)

sd(dem_house_record$party_call_count)
sd(dem_house_record$noncall_count)
sd(dem_house_record$gray_vote_count)

# republican table
rep_majority <- c(104:109, 112)
rep_house_record <- house_coding_record[congress %in% rep_majority, ]
xtable(rep_house_record)

# summary stats
sum(rep_house_record$party_call_count)
sum(rep_house_record$noncall_count)
sum(rep_house_record$gray_vote_count)

mean(rep_house_record$party_call_count)
mean(rep_house_record$noncall_count)
mean(rep_house_record$gray_vote_count)

sd(rep_house_record$party_call_count)
sd(rep_house_record$noncall_count)
sd(rep_house_record$gray_vote_count)

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

  dem_yea_DT <- vote_data[party == "D", .(dem_yea_perc = mean(y, na.rm = TRUE)),
    by = c("vt")]
  rep_yea_DT <- vote_data[party == "R", .(rep_yea_perc = mean(y, na.rm = TRUE)),
    by = c("vt")]
  yea_DT <- merge(dem_yea_DT, rep_yea_DT, by = "vt", all = TRUE)
  yea_DT[dem_yea_perc > rep_yea_perc, yea_party := "D"]
  yea_DT[dem_yea_perc < rep_yea_perc, yea_party := "R"]
  yea_DT[dem_yea_perc == rep_yea_perc, yea_party := "neither"]

  vote_data <- merge(lopside_DT, yea_DT, by = "vt")
  vote_data[, congress := congress_number]
  setnames(vote_data, "vt", "voteno")

  # if(chamber == "house") {
  #   party_call_list <- get_party_calls(party_calls[[paste0("hou", congress_number)]],
  #     n_iterations = 5)
  #   noncall_list <- get_noncalls(party_calls[[paste0("hou", congress_number)]],
  #     n_iterations = 5)
  #   gray_vt_list <- get_gray_votes(party_calls[[paste0("hou", congress_number)]],
  #     n_iterations = 5)
  # } else if (chamber == "senate") {
  #   party_call_list <- get_party_calls(senate_party_calls[[paste0("sen", congress_number)]],
  #     n_iterations = 5)
  #   noncall_list <- get_noncalls(senate_party_calls[[paste0("sen", congress_number)]],
  #     n_iterations = 5)
  #   gray_vt_list <- get_gray_votes(senate_party_calls[["sen", congress_number]],
  #     n_iterations = 5)
  # }
  # vote_data[, party_call := 1 * (vt %in% party_call_list)]
  # vote_data[, noncall := 1 * (vt %in% noncall_list)]
  # vote_data[, gray := 1 * (vt %in% gray_vt_list)]
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


# get summary stats for lopsided votes
table(hou_lop_coding$lopsided, hou_lop_coding$coding)
