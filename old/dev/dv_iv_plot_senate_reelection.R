library(partycalls)
library(ggplot2)

theme_set(theme_bw())

load("test_data/senate_data_lm.RData")

senate_data <- senate_data[drop == 0, ]
senate_data[maj >= .5, maj := 1]
senate_data[maj < .5, maj := 0]

senate_data[, up_for_reelection := as.factor(up_for_reelection)]
senate_data[, majority := as.factor(maj)]

senate_dem <- senate_data[caucus == "Democrat", ]
senate_rep <- senate_data[caucus == "Republican", ]
senate_maj <- senate_data[maj == 1, ]
senate_min <- senate_data[maj == 0, ]

# party
ggplot(senate_dem, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)
ggsave("plots/senate_dem_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm)
ggsave("plots/senate_rep_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_dem, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_dem_iv-dv_reelection_loess.pdf")

dev.off()

ggplot(senate_rep, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_rep_iv-dv_reelection_loess.pdf")

dev.off()

# majority
ggplot(senate_maj, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_maj_iv-dv_reelection_loess.pdf")

dev.off()

ggplot(senate_min, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_min_iv-dv_reelection_loess.pdf")

dev.off()

ggplot(senate_maj, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm)
ggsave("plots/senate_maj_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_min, aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm)
ggsave("plots/senate_min_iv-dv_reelection_lm.pdf")

dev.off()

# dems x majority status
ggplot(senate_dem[maj == 1], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)
ggsave("plots/senate_maj_dem_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_dem[maj == 0], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=lm)
ggsave("plots/senate_min_dem_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_dem[maj == 1], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_maj_dem_iv-dv_reelection_loess.pdf")

dev.off()

ggplot(senate_dem[maj == 0], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("blue2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_min_dem_iv-dv_reelection_loess.pdf")

dev.off()

# reps x majority status
ggplot(senate_rep[maj == 1], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm, se=TRUE)
ggsave("plots/senate_maj_rep_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_rep[maj == 0], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=lm)
ggsave("plots/senate_min_rep_iv-dv_reelection_lm.pdf")

dev.off()

ggplot(senate_rep[maj == 1], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_maj_rep_iv-dv_reelection_loess.pdf")

dev.off()

ggplot(senate_rep[maj == 0], aes(ideological_extremism, pirate100, color = up_for_reelection)) +
  geom_point(shape = 16, alpha = .75) +
  scale_color_manual(breaks = c("0", "1"),
    values = c("red2", "gray25")) +
  geom_smooth(method=loess)
ggsave("plots/senate_min_rep_iv-dv_reelection_loess.pdf")

dev.off()
