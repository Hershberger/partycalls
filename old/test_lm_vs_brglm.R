library(partycalls)

party_calls_brglm <- lapply(93:112,
  partycalls::code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05,  type = "brglm")
names(party_calls_brglm) <- paste0("sen", 93:112)


party_calls_lm <- lapply(93:112,
  partycalls::code_party_calls_by_congress_number,
  chamber = "senate", pval_threshold = 0.05,  type = "lm")
names(party_calls_lm) <- paste0("sen", 93:112)

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
  setnames(lopside_DT, "vt", "voteno")
  lopside_DT[, lopsided := "close"]
  lopside_DT[yea_perc >= 0.65 | yea_perc <= 0.35, lopsided := "lopsided"]
  lopside_DT[, congress := congress_number]
}

levels_party_call <- c("party call", "noncall", "gray")
levels_lopsided <- c("lopsided", "close")

sen_lop_coding <- lapply(93:112, get_vote_data, chamber = "senate")
sen_lop_coding <- rbindlist(sen_lop_coding)
setDT(sen_lop_coding)
sen_lop_coding[, lopsided := factor(lopsided, levels = levels_lopsided)]

brglm_coding <- list()
for (i in 93:112) {
  rc <- paste0("sen", i)
  brglm_coding[[rc]] <- get_party_call_coding(party_calls_brglm[[rc]],
    n_iterations = 5)
  brglm_coding[[rc]]$congress <- i
}

brglm_coding <- rbindlist(brglm_coding)
setDT(brglm_coding)
brglm_coding <- merge(brglm_coding, sen_lop_coding, all.x = TRUE, all.y = FALSE)
brglm_coding[, coding := factor(coding, levels = levels_party_call)]

lm_coding <- list()
for (i in 93:112) {
  rc <- paste0("sen", i)
  lm_coding[[rc]] <- get_party_call_coding(party_calls_lm[[rc]],
    n_iterations = 5)
  lm_coding[[rc]]$congress <- i
}

lm_coding <- rbindlist(lm_coding)
setDT(lm_coding)
lm_coding <- merge(lm_coding, sen_lop_coding, all.x = TRUE, all.y = FALSE)
lm_coding[, coding := factor(coding, levels = levels_party_call)]

# make 2x2 for lopsided/close vs. calls/noncalls for
# 1. brglm
table(brglm_coding$lopsided, brglm_coding$coding)

# 2. lm
table(lm_coding$lopsided, lm_coding$coding)
