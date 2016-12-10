library(data.table)
library(rvest)
years <- 1968:2014
fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
  "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
  "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
  "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
  "49", "50", "51", "53", "54", "55", "56")
state_years <- CJ(year = years, fips = fips)
state_years[year %in% seq(1970, 2012, 6), class := 1]
state_years[year %in% seq(1972, 2014, 6), class := 2]
state_years[year %in% seq(1968, 2016, 6), class := 3]

state_years[, url := paste0("http://uselectionatlas.org/RESULTS/state.php?fips=",
  fips, "&year=", year, "&f=0&off=3&elect=0&class=", class)]
# x <- pbapply::pblapply(state_years$url, function(url) {
#   text <- html_text(read_html(url))
#   if (substr(text, 1, 25) != "\nWarning:  mysql_result()") {
#     start <- regexpr("SenatorialCandidatePoliticalPartyPopular Vote", text)
#     stop <- regexpr("Map Key", text)
#     substr(text, start, stop)
#   } else {
#     NA
#   }
# })
# save(x, file = "inst/extdata/election-atlas-senate.Rdata")
load("inst/extdata/election-atlas-senate.Rdata")
# opponent of Alan CranstonDemocratic was H.L. "Bill" Richardson
# old_x305 <- x[[305]]
# x[[305]] <- paste0("SenatorialCandidatePoliticalPartyPopular Vote\n\n\n\n  \n ",
#   "  Alan CranstonDemocratic\n   3,693,160\n   60.52%\n  \n   ",
#   "H. L. \"Bill\" RichardsonRepublican\n   2,210,267\n   36.22%\n  \n ",
#   "  Jack McCoyAmerican Independent\n   199,005\n   3.26%\n\nM")

party_list <- paste0(c( "Pacific Green", "Independent Green", "IA Green",
  "Green CO", "Wisconsin Green", "Vermont Green", "Green",
  "AK Libertarian", "Independent Reform",
  "Independent Party of Delaware", "Alaskan Independence",
  "LA Democratic", "LA Republican", "United Citizens", "Legalize Marijuana",
  "American Independent Party", "Marijuana Reform", "Citizens First",
  "Conservative IL", "Nom\\. By Petition", "Nominated by Petition",
  "Perot\\'s Independents", "Christian Pro-Life", "Personal Choice",
  "Concerns of People", "Right to Life", "CO Prohibition",
  "Independent-Republican", "American Constitution",
  "Democratic-Farmer-Labor", "Alabama Conservative", "Socialist Worker",
  "Ind\\.-Republican", "Nom Petition", "New Union Party",
  "AL Conservative", "Nat'l Democratic", "AL Prohibition",
  "Alabama Prohibition", "Public Interest Independent",
  "Alabama National Democratic", "American Independent", "God We Trust",
  "Independent American", "Dodd Independent", "Concerned Citizens",
  "Republican", "Democratic", "Independent", "Concerns of the People",
  "Pacific Green", "Unity Party of Colorado", "Statesman",
  "Bob Quast for Term Limits", "Consumer", "Chemical Farming Banned",
  "Libertarian", "Socialist Workers", "Constitution", "Mountain",
  "By Petition", "Petition", "Economic Growth", "Independence",
  "US Taxpayers", "Democratic-F\\.L\\.", "Not Affiliated", "Reform",
  "Country", "VoteKISS", "Peace and Prosperity", "Liberty Union",
  "UT Justice", "Nonparty", "Democratic-NPL", "Common Sense",
  "Ind\\. American", "Jersey Strong Ind\\.", "Ind\\. For Liberty",
  "United States Marijuana", "Unaffiliated", "Ind\\. Amercian",
  "MN Open Prog\\.", "Non-Party", "Ind\\. For Maine",
  "No Party Affiliation", "Ind\\. Of Delaware", "Socialist",
  "US Marijuana", "Progressive", "Working Families", "American",
  "Taxpayers Revival Ticket", "Conservative", "Prohibition",
  "Socialist Labor", "Peace & Freedom", "Buffalo", "Socialist Workers Party",
  "Peace and Freedom", "Industrial Government", "Human Rights",
  "U\\.S\\. Labor", "Communist", "George Wallace", "Democratic-N\\.P\\.L\\.",
  "American Party of Minnesota", "Citizens", "People Over Politics",
  "New Alliance", "Workers World", "Vermont Grassroots", "Grassroots",
  "Natural Law", "Tisch Independent", "Democratic-F\\.-L\\.",
  "Workers Against Concessions", "Grass Roots", "Other Party", "Populist",
  "Freedom for LaRouche", "For the Little Guy", "Keep America First",
  "Republican/Tax Cut Now", "Right To Life", "Independence Fusion",
  "Property Rights", "Patriot", "U\\.S\\. Taxpayers", "Taxpayers", "Pacific",
  "Term Limits", "Constitutional", "Timesizing Not Downsizing",
  "Unenrolled", "Independent \\(Grn\\)", "Nonpartisan", "CT for Lieberman",
  "Veterans Party of America", "Liberty Union", "Marijuana",
  "American Ind\\.", "Solidarity, Defend Life", "Desert Greens",
  "Anti-Bushist Candidate", "Constitution IL", "Other", "Nebraska",
  "Poor People's Campaign", "Boss for Senate", "Non-Affiliated", "No Party",
  "Tea Party", "I\\.D\\.E\\.A\\.",
  "Write-in", "Write-ins-", "Other \\(\\+\\) -", "None of these Candidates-"),
  "$")

x2 <- x
for (i in seq_along(x)) {
  xx <- x[[i]]
  if (xx %in% c("", NA)) {
    x2[[i]] <- NA
  } else if (!is.na(xx)) {
    xx <- substr(xx, 56, nchar(xx) - 3)
    xx <- trimws(strsplit(xx, "\\n")[[1]])
    if (length(xx) > 4) {
      xx <- xx[(1:length(xx))[-seq(4, length(xx), 4)]]
    }
    xx <- t(array(xx, dim = c(3, length(xx) / 3)))
    x2[[i]] <- xx
    parties <- rep("Other", nrow(xx))
      parties[grep("[A-Za-z\\.]Republican", xx[, 1])] <- "Republican"
      parties[grep("[A-Za-z\\.]Democratic", xx[, 1])] <- "Democratic"
      for (party in party_list) {
        xx <- gsub(party, "", xx)
      }

      year <- rep(state_years[i, year], nrow(xx))
      fips <- rep(state_years[i, fips], nrow(xx))
      x2[[i]] <- cbind(xx, parties, year, fips)
  }
}
senate_elections <- rbindlist(lapply(x2, function(xx2) {
  if (is.na(xx2[1])) {
    NULL
  } else {
    as.data.table(xx2)
  }
}))
senate_elections[, V2 := as.numeric(gsub(",", "", V2))]
senate_elections[, V3 := as.numeric(gsub("%", "", V3))]
setnames(senate_elections, c("V1", "V2", "V3"), c("candname",
  "vote", "votepct"))
senate_elections[, vote_rank := rank(-vote), .(fips, year)]
senate_elections <- merge(senate_elections, states, by = "fips")
senate_elections[year %in% seq(1970, 2012, 6), class := 1]
senate_elections[year %in% seq(1972, 2014, 6), class := 2]
senate_elections[year %in% seq(1968, 2016, 6), class := 3]
senate_elections[, year := as.numeric(year)]
senate_elections[, congress1 := 1 + (year - 1788) / 2]
senate_elections[, congress2 := 2 + (year - 1788) / 2]
senate_elections[, congress3 := 3 + (year - 1788) / 2]



# missing elections


save(senate_elections, file = "inst/extdata/senate_elections.Rdata")