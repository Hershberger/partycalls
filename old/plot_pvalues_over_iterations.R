library(ggplot2)
library(data.table)
load("test_data/house_party_calls_flipflop.RData")
congress <- 11
record_of_pvals <- house_party_calls[[congress]]$record_of_pvals
n_iterations <- length(record_of_pvals)
n_rollcalls <- length(record_of_pvals[[1]])
DATA <- CJ(iteration = 1:n_iterations, rollcall_id = 1:n_rollcalls)
DATA[, pval := do.call(c, record_of_pvals)]
ggplot(DATA, aes(iteration, pval, group = rollcall_id)) +
  geom_line(alpha = .01) + theme_bw()
