library(partycalls)

load("test_data/senate_party_calls_emIRT_only.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

senate_coding_record <- data.table(congress = 93:112)
senate_coding_record[, party_call_count := sapply(93:112, function(x)
  length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, noncall_count := sapply(93:112, function(x)
  length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, gray_vote_count := sapply(93:112, function(x)
  length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))]

sum(senate_coding_record$party_call_count)
sum(senate_coding_record$noncall_count)
sum(senate_coding_record$gray_vote_count)

# compare by majority party
dem_majority <- c(93:96, 100:103, 110:112)
rep_majority <- c(97:99, 104:106, 108:109)

dem_senate_record <- senate_coding_record[congress %in% dem_majority, ]
rep_senate_record <- senate_coding_record[congress %in% rep_majority, ]

sum(dem_senate_record$party_call_count)
sum(rep_senate_record$party_call_count)

sum(dem_senate_record$noncall_count)
sum(rep_senate_record$noncall_count)

sum(dem_senate_record$gray_vote_count)
sum(rep_senate_record$gray_vote_count)


# for more in depth coding info
get_vote_data <- function(chamber, congress_number, party_calls) {
  if (chamber == "house") {
    rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  } else if (chamber == "senate") {
    rc <- get(paste0("sen", congress_number))
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
}

sen93_coding <- get_vote_data(chamber = "senate", congress_number = 93)
sen93_party_call <- get_party_calls(senate_party_calls$sen93)
sen93_noncall <- get_party_calls(senate_party_calls$sen93)
sen93_gray <- get_gray_votes(senate_party_calls$sen93)

##############################################
## TODO HERSHBERGER: GET THIS TO WORK RIGHT ##
##############################################

sen93_coding[, gray := 0]
sen93_coding[vt %in% sen93_gray, gray := 1]
sen93_coding[, party_call := 0]
sen93_coding[vt %in% sen93_party_call, party_call := 1]
sen93_coding[, noncall := 0]
sen93_coding[vt %in% sen93_noncall, noncall := 1]
sen93_coding[, dropped := 1]
sen93_coding[gray == 1, dropped := 0]
sen93_coding[party_call == 1, dropped := 0]
sen93_coding[noncall == 1, dropped := 0]

# need to make a table 1 style thing

# vote coding:    | party call | noncall | gray vote
# --------------------------------------------------
# lopsided votes: |            |         |
# close votes:    |            |         |
# majority dems:  |            |         |
# majority reps:  |            |         |
# --------------------------------------------------
# substantive? procedural?
