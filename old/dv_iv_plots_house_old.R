library(partycalls)
library(ggplot2)

theme_set(theme_bw())

old_whoheeds13 <-
  foreign::read.dta("inst/extdata/who-heeds-replication-archive.dta")

setDT(old_whoheeds13)

old_whoheeds13[, south := as.factor(south)]
old_whoheeds13[, maj := as.factor(maj)]

house_dem <- old_whoheeds13[dem == 1, ]
house_rep <- old_whoheeds13[dem == 0, ]


# save as plots/house_old_dem_iv-dv_2_south.pdf
ggplot(house_dem, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/house_old_dem_iv-dv_2_maj.pdf
ggplot(house_dem, aes(ideological_extremism, pirate100, color = maj)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/house_old_rep_iv-dv_2_maj.pdf
ggplot(house_rep, aes(ideological_extremism, pirate100, color = maj)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)

# save as plots/house_old_rep_iv-dv_2_south.pdf
ggplot(house_rep, aes(ideological_extremism, pirate100, color = south)) +
  geom_point(shape = 16) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess, se=FALSE)
