library(partycalls)
library(data.table)
library(yaml)
options(stringsAsFactors = FALSE)

# load party calls data
load("test_data/senate_party_calls_replication_emIRT_only.RData")
names(senate_party_calls) <- paste0("sen", 93:112)


# load legislative effectiveness data
les_data <- read.csv("inst/extdata/93_113_senate_variables.csv")
setDT(les_data)

# get ideal points
new_partycalls <- rbindlist(lapply(senate_party_calls, function(x) data.table(
  congress = gsub("[A-Za-z:/\\.]", "", x$source),
  voteno = x$party_call_coding$voteno,
  new_coding = x$party_call_coding$coding)))

# get responsiveness rates
new_responsiveness <- rbindlist(lapply(93:112, function(congress) {
  cat(congress, " ")
  rc <- make_member_year_data(congress, senate_party_calls, chamber = "senate")
  DATA <- rc$member_year_data
  DATA[, .(congress, icpsr = icpsrLegis, new_ideal_point = pf_ideal,
    new_pirate100 = 100 * responsiveness_party_calls,
    new_pfrate100 = 100 * responsiveness_noncalls,
    new_distance_from_floor_median = dist_from_floor_median,
    new_ideological_extremism = ideological_extremism)]
}))

new_whoheeds13 <- merge(les_data, new_responsiveness,
  by = c("congress", "icpsr"), all = TRUE)

setnames(new_whoheeds13, "st_name", "stabb")
setnames(new_whoheeds13, "icpsr", 'icpsrLegis')
senator_data <- new_whoheeds13

# Populate south indicators
south11 <- c("SC", "MS", "FL", "AL", "AR", "GA", "LA", "TX", "VA", "TN", "NC")
south13 <- c(south11, "OK", "KY")
south17 <- c(south13, "DE", "WV", "MD", "MO")
senator_data[, south11 := 1 * (stabb %in% south11)]
senator_data[, south13 := 1 * (stabb %in% south13)]
senator_data[, south17 := 1 * (stabb %in% south17)]

# Merge in govtrack covarates
source("package/get_govtrack_legislators_csv.R")
senator_data <- merge(senator_data, legislators, by = "icpsrLegis")

# Populate class
source("package/get_govtrack_legislators_yaml_data.R")
senator_data <- merge(senator_data, legislators_yaml, by = "icpsrLegis")

# Populate years of service
source("package/get_bios.R")
senator_data[, years_of_service := substr(bios,
  regexpr("Senate Years of Service", bios) + 25, regexpr("Party", bios) - 2)]
senator_data[, years_of_service := gsub(",", ";", years_of_service)]
senator_data[icpsrLegis == 9369, years_of_service := "1954-2003"]
senator_data[icpsrLegis == 10802, years_of_service := "1965-1983"]

# Fix senators that served discontinuous terms
# for these, create two icpsrLegis & mc tags, one for each stint
# to do so, break up senator_data into three pieces:
# _1 is the ones that are already fine
# _2 is the first stints
# _3 is the second stints
# fix _2 and _3, meaning fix years of service, and
# change mc & icpsrLegis to reflect stint
# mc will have "XX time in senate" pasted to the end
# icpsrLegis will have 100000 or 200000 added
# rbind together and replace senator_data
# finally, change icpsrLegis for the related lines of senator_year_data
icpsrLegis_to_change <- senator_data[grep(";", years_of_service), icpsrLegis]
senator_data_1 <- senator_data[!icpsrLegis %in% icpsrLegis_to_change]
senator_data_2 <- senator_data[icpsrLegis %in% icpsrLegis_to_change]
senator_data_3 <- copy(senator_data_2)
senator_data_2[, mc := paste(mc, "1st time in senate")]
senator_data_2[,
  years_of_service := substring(years_of_service, 1,
    regexpr(";", years_of_service) - 1)]
senator_data_2[icpsrLegis == 2087, class := 3]
senator_data_2 <- senator_data_2[icpsrLegis != 2822] # drop stint before 1973
senator_data_2 <- senator_data_2[icpsrLegis != 3658] # drop stint before 1973
senator_data_2 <- senator_data_2[icpsrLegis != 4728] # drop stint before 1973
senator_data_2[icpsrLegis == 14073, class := 3]
senator_data_2[icpsrLegis == 14806, class := 3]
senator_data_2[icpsrLegis == 14904, class := 3]
senator_data_2[icpsrLegis == 14914, class := 1]
senator_data_2[icpsrLegis == 15502, class := 3]
senator_data_3[, mc := paste(mc, "2nd time in senate")]
senator_data_3[,
  years_of_service := substring(years_of_service,
    regexpr(";", years_of_service) + 2, 100)]
senator_data_3[icpsrLegis == 2087, class := 3]
senator_data_3[icpsrLegis == 2822, class := 2]
senator_data_3[icpsrLegis == 3658, class := 3]
senator_data_3[icpsrLegis == 4728, class := 1]
senator_data_3[icpsrLegis == 14073, class := 1]
senator_data_3[icpsrLegis == 14806, class := 3]
senator_data_3[icpsrLegis == 14904, class := 1]
senator_data_3[icpsrLegis == 14914, class := 2]
senator_data_3[icpsrLegis == 15502, class := 1]
senator_data_2[, icpsrLegis := 100000 + icpsrLegis]
senator_data_3[, icpsrLegis := 200000 + icpsrLegis]
senator_data <- rbind(senator_data_1, senator_data_2, senator_data_3)
senator_year_data[icpsrLegis == 2087 & congress <= 93, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 2087 & congress >= 94, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 2822 & congress <= 77, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 2822 & congress >= 78, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 3658 & congress <= 88, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 3658 & congress >= 91, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 4728 & congress <= 88,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 4728 & congress >= 92, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14073 & congress <= 93 ,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14073 & congress >= 95, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14806 & congress <= 105,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14806 & congress >= 112, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14904 & congress <= 100,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14904 & congress >= 101, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14914 & congress <= 107,  `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 14914 & congress >= 108, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]
senator_year_data[icpsrLegis == 15502 & congress <= 102, `:=`(
  mc = paste(mc, "1st time in senate"), icpsrLegis = 100000 + icpsrLegis)]
senator_year_data[icpsrLegis == 15502 & congress >= 103, `:=`(
  mc = paste(mc, "2nd time in senate"), icpsrLegis = 200000 + icpsrLegis)]

# Populate died_in_office, defeated_for_renomination, defeated_for_reelection,
#   changed_party_affiliation, resigned, did_not_seek_reelection
source("package/mine_bios.R")

# Populate up_for_reelction
senator_year_data[, up_for_reelection := 0]
senator_year_data[(class == 1 & congress %in% seq(1, 120, 3)) |
    (class == 2 & congress %in% seq(2, 120, 3)) |
    (class == 3 & congress %in% seq(3, 120, 3)),
  up_for_reelection := 1]

save(new_whoheeds13,
  file = "data/senate_party_calls_replication_emIRT_only.RData")

