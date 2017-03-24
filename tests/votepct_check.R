load("test_data/new_whoheeds13_lm.RData")

load("inst/extdata/who-heeds-replication-archive130102.rdata")
setDT(d)
summary(d$votepct)
setnames(d, "icpsr", "icpsrLegis")

new_whoheeds13 <- new_whoheeds13[congress <= 109,]

test <- merge(new_whoheeds13, d, by = c("congress", "icpsrLegis"))

plot(test$votepct.x, test$votepct.y)
