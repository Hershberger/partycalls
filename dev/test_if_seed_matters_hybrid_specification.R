load("test_data/house_party_calls_hybrid_seed1.RData")
hybrid_seed1 <- rbindlist(lapply(house_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

load("test_data/house_party_calls_hybrid_seed2.RData")
hybrid_seed2 <- rbindlist(lapply(house_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

load("test_data/house_party_calls_annealing_seed1.RData")
sim_annealing_seed1 <- rbindlist(lapply(house_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

load("test_data/house_party_calls_annealing_seed2.RData")
sim_annealing_seed2 <- rbindlist(lapply(house_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

X <- merge(hybrid_seed1, hybrid_seed2, by = c("congress", "voteno"))
table_x <- X[, table(new_coding.x, new_coding.y)]
table_x
chisq.test(table_x)

Y <- merge(hybrid_seed1, sim_annealing_seed1, by = c("congress", "voteno"))
table_y <- Y[, table(new_coding.x, new_coding.y)]
table_y
chisq.test(table_y)

Z <- merge(hybrid_seed2, sim_annealing_seed2, by = c("congress", "voteno"))
table_z <- Z[, table(new_coding.x, new_coding.y)]
table_z
chisq.test(table_z)
