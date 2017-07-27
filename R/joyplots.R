library(partycalls)
library(ggplot2)
scale01 <- function(x) (x - min(x)) / (max(x) - min(x))

add_densities_to_ggplot <- function(g, x, m, PARTY, CHAMBER, max = NULL,
  alpha_level = .95)
{
  f <- density(x)
  f$y <- 1.6 * scale01(f$y)
  if (!is.null(max)) {
    ok <- which(f$x <= max)
    f$x <- f$x[ok]
    f$y <- f$y[ok]
  }
  zero_line <- m
  f$y <- f$y + zero_line
  DATA <- data.frame(x = f$x, y = f$y,
    party = PARTY, chamber = CHAMBER)
  DATA2 <- data.frame(
    x = c(f$x, rev(f$x)),
    y = c(f$y, rep(zero_line, length(f$y))),
    party = PARTY, chamber = CHAMBER)
  if (PARTY == "Democrats") {
    col <- "lightblue"
    fil <- ifelse(m %% 2 == 0, "#436EEE", "#5190ED")
  } else {
    col <- "#FF6666"
    fil <- ifelse(m %% 2 == 0, "#CC1100", "#EE5C42")
  }

  g + geom_line(data = DATA, aes(x, y), color = col) +
    geom_polygon(data = DATA2, aes(x, y), fill = fil, alpha = alpha_level)
}


g <- ggplot()
for (m in 20:1) {
  x <- partycalls::house_data[congress == 113 - m & dem == 1 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls]
  g <- add_densities_to_ggplot(g, x, m, "Democrats", "House", max = 100)

  x <- partycalls::house_data[congress == 113 - m & dem == 0 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls]
  g <- add_densities_to_ggplot(g, x, m, "Republicans", "House", max = 100)

  x <- partycalls::senate_data[congress == 113 - m & dem == 1 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls]
  g <- add_densities_to_ggplot(g, x, m, "Democrats", "Senate", max = 100)

  x <- partycalls::senate_data[congress == 113 - m & dem == 0 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls]
  g <- add_densities_to_ggplot(g, x, m, "Republicans", "Senate", max = 100)
}
g + facet_grid(chamber ~ party) +
  scale_y_continuous(breaks = 1:20, labels = seq(2011, 1973, -2)) +
  theme_minimal() + ylab("") + xlab("") +
  xlim(60, 100) +
  scale_x_continuous(trans = scales::boxcox_trans(2)) +
  ggtitle("Responsiveness to Party Calls")


g <- ggplot() + geom_vline(xintercept = 0, color = "lightgray", linetype = 3)
for (m in 20:1) {
  x <- partycalls::house_data[congress == 113 - m & dem == 1 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls - baseline_rate]
  g <- add_densities_to_ggplot(g, x, m, "Democrats", "House")

  x <- partycalls::house_data[congress == 113 - m & dem == 0 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls - baseline_rate]
  g <- add_densities_to_ggplot(g, x, m, "Republicans", "House")

  x <- partycalls::senate_data[congress == 113 - m & dem == 1 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls - baseline_rate]
  g <- add_densities_to_ggplot(g, x, m, "Democrats", "Senate")

  x <- partycalls::senate_data[congress == 113 - m & dem == 0 &
      !is.na(responsiveness_to_party_calls),
    responsiveness_to_party_calls - baseline_rate]
  g <- add_densities_to_ggplot(g, x, m, "Republicans", "Senate")
}
g + facet_grid(chamber ~ party) +
  scale_y_continuous(breaks = 1:20, labels = seq(2011, 1973, -2)) +
  theme_minimal() + ylab("") + xlab("") + xlim(-40, 40) +
  ggtitle("Responsiveness to Party Calls")
