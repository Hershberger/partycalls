library(partycalls)
library(ggplot2)
library(gridExtra)
library(xtable)

library(extrafont)
loadfonts()

# load data for analysis
load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]
senate_data[, vote_share := vote_share * 100]
senate_data[, pres_vote_share := pres_vote_share * 100]
load("test_data/house_party_calls_lm.RData")
names(house_party_calls) <- paste0("hou", 93:112)
load("test_data/senate_party_calls_lm.RData")
names(senate_party_calls) <- paste0("sen", 93:112)

# subsets of the data
house_coding_record <- data.table(congress = 93:112)
house_coding_record[, party_calls := sapply(93:112, function(x)
  length(get_party_calls(house_party_calls[[paste0("hou", x)]])))]
house_coding_record[, noncalls := sapply(93:112, function(x)
  length(get_noncalls(house_party_calls[[paste0("hou", x)]])))]
house_coding_record[, gray_vote_count := sapply(93:112, function(x)
  length(get_gray_votes(house_party_calls[[paste0("hou", x)]])))]
house_coding_record[, percent_party_calls :=
    100 * party_calls / (party_calls + noncalls)]
house_coding_record[, majority := "Republican"]
house_coding_record[congress %in% c(93:103, 110:111), majority := "Democrat"]

senate_coding_record <- data.table(congress = 93:112)
senate_coding_record[, party_calls := sapply(93:112, function(x)
  length(get_party_calls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, noncalls := sapply(93:112, function(x)
  length(get_noncalls(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, gray_vote_count := sapply(93:112, function(x)
  length(get_gray_votes(senate_party_calls[[paste0("sen", x)]])))]
senate_coding_record[, percent_party_calls :=
    100 * party_calls / (party_calls + noncalls)]
senate_coding_record[, majority := "Republican"]
senate_coding_record[congress %in% c(93:96, 100:103, 107, 110:112),
  majority := "Democrat"]

# Make Figure 1

house_coding_record[, chamber := "House"]
senate_coding_record[, chamber := "Senate"]
coding_record <- rbind(house_coding_record, senate_coding_record)


pdf(file = "drafts/party_call_percent_both.pdf",
  width = 6, height = 6)
ggplot(coding_record, aes(congress, percent_party_calls,
  color = as.factor(majority))) +
  ylim(30, 90) +
  xlab("Congress") +
  ylab("Percentage of Votes") +
  ggtitle("Frequency of Party Calls over Time") +
  geom_point(aes(shape = as.factor(majority), size = ifelse(majority == "Democrat", 3, 2.25))) +
  scale_shape_manual("Majority Party", values = c(16, 17), guide = FALSE) +
  scale_color_manual("Majority Party", values = c("blue3", "red3"), guide = FALSE) +
  scale_size_continuous(limits = c(2, 3)) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(family = "CM Roman"),
    legend.position = "none") +
  facet_wrap(~ chamber)
dev.off()
embed_fonts("drafts/party_call_percent_both.pdf",
  outfile = "drafts/party_call_percent_both.pdf")



# load data for analysis
load("test_data/senate_data_lm.RData")
senate_data <- senate_data[drop == 0, ]
senate_data[congress == 107 & caucus == "Republican", maj := 0]
senate_data[congress == 107 & caucus == "Democrat", maj := 1]

load("test_data/new_whoheeds13_lm.RData")
new_whoheeds13 <- new_whoheeds13[drop == 0, ]
new_whoheeds13[, vote_share := vote_share - mean(vote_share, na.rm = TRUE)]
new_whoheeds13[is.na(vote_share) == TRUE, vote_share := 0]

sen_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share +
  female + afam + latino + up_for_reelection +
  seniority + freshman + retiree + best_committee + leader +
  power_committee + chair
s_extremism <- function(i, j) {
  summary(lm(sen_extremism,
    data = subset(senate_data, congress == i & maj == j)),
    vcov = vcovHC(type = "HC1"))
}

B_s <- SE_s <- data.frame(row.names = 93:112)
B_s$extremism_maj <- sapply(93:112, function(x)
  s_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B_s$extremism_min <- sapply(93:112, function(x)
  s_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE_s$extremism_maj <- sapply(93:112, function(x)
  s_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE_s$extremism_min <- sapply(93:112, function(x)
  s_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

hou_extremism <- pirate100 ~ ideological_extremism +
  pfrate100 + pres_vote_share + south + vote_share + female + afam + latino +
  seniority + freshman + bestgrosswart + leader +
  power + chair
h_extremism <- function(i, j) {
  summary(lm(hou_extremism,
    data = subset(new_whoheeds13, congress == i & majority == j)),
    vcov = vcovHC(type = "HC1"))
}

B_h <- SE_h <- data.frame(row.names = 93:112)
B_h$extremism_maj <- sapply(93:112, function(x)
  h_extremism(x, 1)$coef["ideological_extremism", "Estimate"])
B_h$extremism_min <- sapply(93:112, function(x)
  h_extremism(x, 0)$coef["ideological_extremism", "Estimate"])
SE_h$extremism_maj <- sapply(93:112, function(x)
  h_extremism(x, 1)$coef["ideological_extremism", "Std. Error"])
SE_h$extremism_min <- sapply(93:112, function(x)
  h_extremism(x, 0)$coef["ideological_extremism", "Std. Error"])

results <- CJ(
  maj = c("Majority", "Minority"),
  chamber = c("Senate", "House"),
  Congress = 93:112)
results[, Estimate := c(
  B_h$extremism_maj,
  B_s$extremism_maj,
  B_h$extremism_min,
  B_s$extremism_min)]
results[, SE := c(
  SE_h$extremism_maj,
  SE_s$extremism_maj,
  SE_h$extremism_min,
  SE_s$extremism_min)]
results[, q025 := Estimate + qnorm(.025) * SE]
results[, q975 := Estimate + qnorm(.975) * SE]
results[, q250 := Estimate + qnorm(.25) * SE]
results[, q750 := Estimate + qnorm(.75) * SE]

results[maj == "Majority", party := "Democrat"]
results[maj == "Minority", party := "Republican"]
results[maj == "Majority" & chamber == "House" &
    Congress %in% house_coding_record[majority == "Republican", congress],
  party := "Republican"]
results[maj == "Minority" & chamber == "House" &
    Congress %in% house_coding_record[majority == "Republican", congress],
  party := "Democrat"]
results[maj == "Majority" & chamber == "Senate" &
    Congress %in% senate_coding_record[majority == "Republican", congress],
  party := "Republican"]
results[maj == "Minority" & chamber == "Senate" &
    Congress %in% senate_coding_record[majority == "Republican", congress],
  party := "Democrat"]


pdf(file="drafts/both-chambers-figure2.pdf", ## RENAME
  width = 6, height = 6)
ggplot(results, aes(Congress, Estimate, color = party, shape = party)) +
  geom_hline(yintercept = 0, color = "gray", linetype = 3) +
  geom_errorbar(aes(ymin = q025, ymax = q975), size = .5, width = 0) +
  geom_errorbar(aes(ymin = q250, ymax = q750), size = 1, width = 0) +
  geom_point(aes(size = ifelse(party == "Democrat", 2.35, 2.125))) +
  facet_grid(maj ~ chamber, scales = "free") +
  theme_minimal() +
  scale_color_manual(values = c("Democrat" = "blue3", "Republican" = "red3")) +
  scale_size_continuous(limits = c(2, 3)) +
  ylab("Coefficient on Ideological Extremism") +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(family = "CM Roman"),
    strip.text = element_text(size = 12),
    legend.position = "none")
dev.off()

embed_fonts("drafts/both-chambers-figure2.pdf",
  outfile = "drafts/both-chambers-figure2.pdf")




# Make Figure 3
# select variables needed
DATA <- senate_data[!is.na(pirate100), .(congress, stabb, class, caucus, maj,
  votes, tr = up_for_reelection, y1 = pirate100, y2 = pfrate100)]
setorder(DATA, stabb, congress, class)

# make dems majority for congress 107
DATA[congress == 107 & caucus == "Democrat", maj := 1]
DATA[congress == 107 & caucus == "Republican", maj := 0]

# subset to cases with two senators, one treated, one control
DATA <- merge(DATA,
  DATA[, .N, .(stabb, congress)],
  by = c("stabb", "congress"),
  all.x = TRUE)
DATA <- DATA[N == 2]
DATA[, mean_tr := mean(tr), .(stabb, congress)]

# Estimate Effects
diff_pi <- DATA[mean_tr == .5,
  sum(tr * y1) - sum((1 - tr) * y1), .(stabb, congress)][,
    mean(V1)]
diff_pf <- DATA[mean_tr == .5,
  sum(tr * y2) - sum((1 - tr) * y2), .(stabb, congress)][,
    mean(V1)]

# Do inference
# bootstrap by state
states <- DATA[, unique(stabb)]

boot <- function(i) {
  boot_states <- sample(states, replace = TRUE)
  boot_DATA <- rbindlist(lapply(seq_along(boot_states), function(boot_id) {
    boot_DATA <- DATA[stabb == boot_states[boot_id]]
    boot_DATA[, boot_id := boot_id]
    boot_DATA
  }))
  boot_diff_pi <- boot_DATA[mean_tr == .5,
    sum(tr * y1) - sum((1 - tr) * y1), .(boot_id, congress)][,
      mean(V1)]
  boot_diff_pf <- boot_DATA[mean_tr == .5,
    sum(tr * y2) - sum((1 - tr) * y2), .(boot_id, congress)][,
      mean(V1)]
  data.table(boot_diff_pi, boot_diff_pf)
}

# boots <- rbindlist(lapply(1:1000, boot))
# save(boots, file = "drafts/boots.RData")
load("drafts/boots.RData")

differences <- data.table(test = c("Party Calls",
  "Party Free Votes"),
  Estimate = c(diff_pi, diff_pf),
  Lower_Bound = c(boots[, quantile(boot_diff_pi, .025)],
    boots[, quantile(boot_diff_pf, .025)]),
  Upper_Bound = c(boots[, quantile(boot_diff_pi, .975)],
    boots[, quantile(boot_diff_pf, .975)]),
  Lower_50 = c(boots[, quantile(boot_diff_pi, 0.25)],
    boots[, quantile(boot_diff_pf, 0.25)]),
  Upper_50 = c(boots[, quantile(boot_diff_pi, 0.75)],
    boots[, quantile(boot_diff_pf, 0.75)])
)

# make coeff plot from effects
# differences[, position := 0]
# differences[test == "Party Free Difference", position := 1]
#
# differences[, test := gsub(" Difference", "", test)]
differences[, test := factor(test, levels = test)]



pdf(file="drafts/senate_difference_estimates.pdf",
  width = 5, height = 4)
ggplot(differences, aes(test, Estimate)) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = 3, color = "gray") +
  geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), width = 0, size = .75) +
  geom_errorbar(aes(ymin = Lower_50, ymax = Upper_50), width = 0, size = 1.5) +
  geom_point(size = 4) +
  xlab("") + ylab("") +
  ggtitle(paste0("Reelection Limits Responsiveness to Party Calls")) +
  theme(plot.title = element_text(hjust = 0.5),
    text = element_text(family = "CM Roman", size = 12))

dev.off()

embed_fonts("drafts/senate_difference_estimates.pdf",
  outfile = "drafts/senate_difference_estimates.pdf")

