

multiple_terms_icpsrLegis <-
  c(22, 134, 394, 395, 425, 437, 525, 751, 765, 777, 854, 1070,
  1100, 1322, 1432, 1434, 1480, 1524, 1550, 1641, 1694, 1785, 1940,
  2269, 2289, 2380, 2738, 2782, 2803, 2898, 3000, 3107, 3127, 3310,
  3362, 3544, 3603, 3697, 3706, 3709, 4266, 4288, 4922, 4940, 5147,
  5299, 5347, 5386, 5402, 5641, 5739, 5746, 5970, 6197, 6330, 6697,
  6787, 7070, 7148, 7237, 7379, 7483, 7656, 7668, 7826, 8379, 8454,
  8476, 8800, 8948, 8962, 9298, 9369, 9398, 9492, 9515, 9526, 9782,
  9793, 9842, 9970, 10400, 10802, 88948, 90802, 91480, 92738, 94288,
  95402, 95970, 97379, 98476, 98948, 99369)


multiple_terms_senator_data_index <- sapply(multiple_terms_icpsrLegis,
  function(iL) which(senator_data$icpsrLegis == iL))

multiple_terms_icpsrLegis_with_gap <- do.call(c,
  lapply(multiple_terms_icpsrLegis, function(iL) {
    ok <- which(senator_data$icpsrLegis == iL)
    congresses_served <- senator_year_data[icpsrLegis == iL, congress]
    diffs <- tail(congresses_served, -1) - head(congresses_served, -1)
    if (any(diffs != 1)) {
      iL
    } else {
      NULL
    }
  }))

# write code for multiple terms with gaps
do.call(c,
  lapply(multiple_terms_icpsrLegis_with_gap, function(iL) {
    ok <- which(senator_data$icpsrLegis == iL)
    congresses_served <- senator_year_data[icpsrLegis == iL, congress]
    diffs <- tail(congresses_served, -1) - head(congresses_served, -1)
    last_cong_before_gap <- congresses_served[which(diffs != 1)]
    first_cong_after_gap <- congresses_served[which(diffs != 1) + 1]
    mc <- senator_data[ok, mc]
    mc_1st <- paste0(mc, " 1st time in senate")
    mc_2nd <- paste0(mc, " 2nd time in senate")
    iL_1st <- 100000 + iL
    iL_2nd <- 200000 + iL
    reason <- ",discontinuous terms"
    out <- c(
      paste0(mc, ",", iL, ",", NA, ",mc,", mc_1st,  reason),
      paste0(mc_1st, ",", iL, "," ,NA, ",icpsrLegis,", iL_1st, reason),
      paste0(mc_1st,",",iL_1st, ",", first_cong_after_gap, ",mc,", mc_2nd,
        reason),
      paste0(mc_2nd,",",iL_1st, ",", first_cong_after_gap, ",icpsrLegis,",
        iL_2nd, reason))
    cat(out, sep = "\n")
  }))

three_periods_of_service <- c(1437, 2052, 3256, 3995, 4001, 4077, 4310, 4987,
  5046, 6565, 6856, 7502, 8470, 9698, 10082, 10129, 10268, 85046, 87502, 95046)

multiple_terms_icpsrLegis_with_two_gaps <- do.call(c,
  lapply(three_periods_of_service, function(iL) {
    ok <- which(senator_data$icpsrLegis == iL)
    congresses_served <- senator_year_data[icpsrLegis == iL, congress]
    diffs <- tail(congresses_served, -1) - head(congresses_served, -1)
    if (any(diffs != 1)) {
      iL
    } else {
      NULL
    }
  }))

do.call(c,
  lapply(multiple_terms_icpsrLegis_with_two_gaps, function(iL) {
    ok <- which(senator_data$icpsrLegis == iL)
    congresses_served <- senator_year_data[icpsrLegis == iL, congress]
    diffs <- tail(congresses_served, -1) - head(congresses_served, -1)
    last_cong_before_gap <- congresses_served[which(diffs != 1)]
    first_cong_after_gap <- congresses_served[which(diffs != 1) + 1]
    last_cong_before_gap_1 <- last_cong_before_gap[1]
    last_cong_before_gap_2 <- last_cong_before_gap[2]
    first_cong_after_gap_1 <- first_cong_after_gap[1]
    first_cong_after_gap_2 <- first_cong_after_gap[2]
    mc <- senator_data[ok, mc]
    mc_1st <- paste0(mc, " 1st time in senate")
    mc_2nd <- paste0(mc, " 2nd time in senate")
    mc_3rd <- paste0(mc, " 3rd time in senate")
    iL_1st <- 100000 + iL
    iL_2nd <- 200000 + iL
    iL_3rd <- 300000 + iL
    reason <- ",discontinuous terms"
    out <- c(
      paste0(mc, ",", iL, ",", NA, ",mc,", mc_1st,  reason),
      paste0(mc_1st, ",", iL, "," ,NA, ",icpsrLegis,", iL_1st, reason),
      paste0(mc_1st,",",iL_1st, ",", first_cong_after_gap_1, ",mc,", mc_2nd,
        reason),
      paste0(mc_2nd,",",iL_1st, ",", first_cong_after_gap_1, ",icpsrLegis,",
        iL_2nd, reason),
      paste0(mc_2nd,",",iL_2nd, ",", first_cong_after_gap_2, ",mc,", mc_3rd,
        reason),
      paste0(mc_3rd,",",iL_2nd, ",", first_cong_after_gap_2, ",icpsrLegis,",
        iL_3rd, reason))
    cat(out, sep = "\n")
  }))

c(9246, 89246)

multiple_terms_icpsrLegis_without_gap <- setdiff(multiple_terms_icpsrLegis,
  multiple_terms_icpsrLegis_with_gap)




c(1437, 2052, 3256, 3995, 4001, 4077, 4310, 4987, 5046, 6565,
  6856, 7502, 8470, 9698, 10082, 10129, 10268, 85046, 87502, 95046)

c(9246, 89246)