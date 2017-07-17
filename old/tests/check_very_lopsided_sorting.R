library(partycalls)
rc <- sen112
rc_check <- rc
library(partycalls)
rc <- sen112
rc_check <- rc
rc <- pscl::dropRollCall(rc, dropList = alist(dropLegis = state == "USA"))
rc <- pscl::dropRollCall(rc, dropList = alist(lop = 4))
rc$m
rc_check$m

rc <- emIRT::convertRC(rc, type = "binIRT")
rc_check <- emIRT::convertRC(rc_check, type = "binIRT")
DT <- CJ(vt = colnames(rc$votes), mc = rownames(rc$votes), sorted = FALSE)
DT$y <- as.vector(rc$votes)
DT$party <- rc$legis.data$party
DT[y %in% c(0, 9), y:= NA]
DT[y == -1, y:= 0]
DT_check <- CJ(vt = colnames(rc_check$votes), mc = rownames(rc_check$votes), sorted = FALSE)
DT_check$y <- as.vector(rc_check$votes)
DT_check$party <- rc_check$legis.data$party
DT_check[y %in% c(0, 9), y:= NA]
DT_check[y == -1, y:= 0]
View(DT)
DT_check[vt %in% DT$vt == FALSE, unique(vt)]

vote_check <- DT_check[vt %in% DT$vt == FALSE, unique(vt)]
noncalls <- sample(rc$m, floor(.5 * rc$m))
noncalls_check <- sample(rc$m, floor(.5 * rc_check$m))
switched_votes <- seq_len(rc$m)

match_counter <- 0
counter <- 0
record_of_coding <- list()
record_of_ideals <- list()
match_switch <- FALSE # for old match checking procedure
record_of_pvals <- list()
record_of_tvals <- list()
countdown_started <- FALSE
tvals <- rep(0, rc$m)
flip_flop_votes <- c()
counter <- counter + 1
record_of_coding[[counter]] <- noncalls
old_noncalls <- noncalls
old_switched_votes <- switched_votes
classification_distance_message <- NULL

rc1_check <- rc_check
rc2_check <- rc_check
rc1 <- rc
rc2 <- rc

votes_for_ideal_point_estimation <- noncalls
votes_for_ideal_point_estimation_check <- noncalls_check

rc1$votes <- rc$votes[, votes_for_ideal_point_estimation]
rc2$votes <- rc$votes[, -votes_for_ideal_point_estimation]

rc1_check$votes <- rc_check$votes[, votes_for_ideal_point_estimation_check]
rc2_check$votes <- rc_check$votes[, -votes_for_ideal_point_estimation_check]

rc1$m <- ncol(rc1$votes)
rc2$m <- ncol(rc2$votes)

rc1_check$m <- ncol(rc1_check$votes)
rc2_check$m <- ncol(rc2_check$votes)

p <- emIRT::makePriors(rc1$n, rc1$m, 1)
s <- emIRT::getStarts(rc1$n, rc1$m, 1)

p_check <- emIRT::makePriors(rc1_check$n, rc1_check$m, 1)
s_check <- emIRT::getStarts(rc1_check$n, rc1_check$m, 1)

sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
  "NUL"
  } else {
    "/dev/null"
    }
sink(sink_target)

l <- emIRT::binIRT(.rc = rc1, .starts = s, .priors = p,
  .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
sink()
unlink(sink_target)

sink_target <- if (Sys.info()[["sysname"]] == "Windows") {
  "NUL"
  } else {
    "/dev/null"
    }
sink(sink_target)

l_check <- emIRT::binIRT(.rc = rc1_check, .starts = s_check, .priors = p_check,
  .control = list(threads = 1, verbose = FALSE, thresh = 1e-6))
sink()
unlink(sink_target)

DT$x <- l$means$x
DT_check$x <- l_check$means$x

ideal <- l$means$x
ideal_check <- l_check$means$x

party <- rc1$legis.data$party
party_check <- rc1_check$legis.data$party

orientation_correct <- mean(ideal[party == "D"]) < mean(ideal[party == "R"])
if (!orientation_correct) {
  ideal <- -ideal
}

orientation_correct_check <- mean(ideal_check[party == "D"]) < mean(ideal_check[party == "R"])
if (!orientation_correct) {
  ideal <- -ideal
}

type <- "brglm"
regs <- DT[party %in% c("D", "R"), test_rollcall(.SD, type), .(vt)]
regs_check <- DT_check[party %in% c("D", "R"), test_rollcall(.SD, type), .(vt)]
regs_check[vt %in% vote_check, p]

regs_check[vt %in% vote_check & is.na(p) == TRUE, length(p)]
regs_check[vt %in% vote_check & p < 0.05, length(p)]
regs_check[vt %in% vote_check & p >= 0.05, length(p)]

View(regs_check)
regs_check[p < 0.05, length(vt)]

regs[p < 0.05, length(vt)]

regs[p <= 0.05, length(vt)]
regs[p <= 0.05, length(vt)]
regs_check[p <= 0.05, length(vt)]
