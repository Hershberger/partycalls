library(partycalls)
library(data.table)
options(stringsAsFactors = FALSE)
load("inst/extdata/houKHfiles001-111.rdata")

h111_test <- h111
h111_test <- pscl::dropRollCall(h111_test,
  dropList = alist(dropLegis = state == "USA"))
h111_test <- emIRT::convertRC(h111_test, type = "binIRT")

h111_test$votes

DT_test <- CJ(vt = colnames(h111_test$votes), mc = rownames(h111_test$votes),
  sorted = FALSE)
DT_test$y <- as.vector(h111_test$votes)
DT_test$party <- h111_test$legis.data$party
DT_test[y %in% c(0, 9), y:= NA]
DT_test[y == -1, y:= 0]
View(DT_test)

DT_test[, yea_perc := mean(y, na.rm = TRUE), by = vt]
noncall_test <- subset(DT_test, yea_perc < 0.65 & yea_perc > 0.35, select = vt)
noncall_test <- c(unique(noncall_test$vt))
noncall_test <-
  as.numeric(c(gsub(pattern = "Vote ", replacement = "", noncall_test)))

rc_test_SD <- data.table(party = c("D", "D", "R", "D", "R"), y = c(1, 0, 1, 0, 1))
length(unique(rc_test_SD[!is.na(y) & party %in% c("D", "R"), y]))
