library(data.table)

load("test_data/house_party_calls_flipflop.RData")
house_party_flipflop <- house_party_calls
house_party_flipflop <- rbindlist(lapply(house_party_flipflop,
  function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

house_party_flipflop$gray <- 0
house_party_flipflop$gray[house_party_flipflop$new_coding == "gray"] <- 1
house_party_flipflop[,
  mean_gray := mean(gray), by = congress]
flip_flop_gray <- unique(house_party_flipflop[, list(congress, mean_gray)])
flip_flop_gray$mean_gray <- flip_flop_gray$mean_gray * 100


load("inst/extdata/house_party_calls_replication.RData")
house_party_annealing <- house_party_calls
house_party_annealing <- rbindlist(lapply(house_party_annealing,
  function(x) data.table(
    congress = gsub("[A-Za-z:/\\.]", "", x$source),
    voteno = x$party_call_coding$voteno,
    new_coding = x$party_call_coding$coding)))

house_party_annealing$gray <- 0
house_party_annealing$gray[house_party_annealing$new_coding == "gray"] <- 1
house_party_annealing[, mean_gray := mean(gray), by = congress]
annealing_gray <- unique(house_party_annealing[,
  list(congress, mean_gray)])
annealing_gray$mean_gray <- annealing_gray$mean_gray * 100


load("test_data/house_party_calls_hybrid_seed1.RData")
house_party_hybrid <- house_party_calls
house_party_hybrid <- rbindlist(lapply(house_party_hybrid,
  function(x) data.table(
    congress = gsub("[A-Za-z:/\\.]", "", x$source),
    voteno = x$party_call_coding$voteno,
    new_coding = x$party_call_coding$coding)))

house_party_hybrid$gray <- 0
house_party_hybrid$gray[house_party_hybrid$new_coding == "gray"] <- 1
house_party_hybrid[, mean_gray := mean(gray), by = congress]
hybrid_gray <- unique(house_party_hybrid[,
  list(congress, mean_gray)])
hybrid_gray$mean_gray <- hybrid_gray$mean_gray * 100

house_party_calls <- data.table(congress = annealing_gray$congress,
  annealing_gray = annealing_gray$mean_gray,
  flip_flop_gray = flip_flop_gray$mean_gray,
  hybrid_gray = hybrid_gray$mean_gray
)

par(mfrow = c(3, 1))

plot(house_party_calls$congress, house_party_calls$annealing_gray,
  ylim = c(0, 25),
  type = "b", xlab = "Congress", ylab = "Percent Gray",
  main = "Old Simulated Annealing")

plot(house_party_calls$congress, house_party_calls$flip_flop_gray,
  ylim = c(0, 25),
  type = "b", xlab = "Congress", ylab = "Percent Gray",
  main = "Flip Flop Vote Sampling")

plot(house_party_calls$congress, house_party_calls$hybrid_gray,
  ylim = c(0, 25),
  type = "b", xlab = "Congress", ylab = "Percent Gray",
  main = "Hybrid Model")

par(mfrow = c(1, 1))
