# library(senatePartyCalls)
library(data.table)
syd <- copy(senator_year_data)

committees_93102 <- read.delim("inst/extdata/senate_assignments_80-102.txt",
  header = FALSE, sep = "\n")
committees_103112 <- read.csv(
  "inst/extdata/senate_assignments_103-11.csv")
committee_values <- fread("inst/extdata/committee_values.csv")

setDT(committees_93102)
# add committee id's and scores here
committees_93102[, `:=`(
  congress = as.numeric(substr(V1, 48, 50)),
  code = as.numeric(substr(V1, 51, 53)),
  icpsrLegis = as.numeric(substr(V1, 15, 19)),
  Senior.Party.Member = as.numeric(substr(V1, 57, 58)),
  name = trimws(substr(V1, 21, 45))
)]
committees_93102[, V1 := NULL]
# repair some errors
committees_93102[name == "Conrad, Kent", icpsrLegis := 15502]
committees_93102[name == "Lautenberg, Frank R.", icpsrLegis := 14914]
committees_93102[name == "Gorton, Slade", icpsrLegis := 14904]
committees_93102[name == "East, John P.", icpsrLegis := 14903]
committees_93102[name == "Hawkins, Paula", icpsrLegis := 14905]
committees_93102[name == "Mattingly, Mack", icpsrLegis := 14906]
committees_93102[name == "Murkowski, Frank H.", icpsrLegis := 14907]
committees_93102[name == "Nickles, Don", icpsrLegis := 14908]
committees_93102[name == "Rudman, Warren B.", icpsrLegis := 14909]
committees_93102[name == "Rudman, Warren", icpsrLegis := 14909]
committees_93102[name == "Specter, Arlen", icpsrLegis := 14910]
committees_93102[name == "Bryan, Richard H.", icpsrLegis := 15700]
committees_93102[name == "Burns, Conrad", icpsrLegis := 15701]
committees_93102[name == "Kerrey, Bob", icpsrLegis := 15702]
committees_93102[name == "Kohl, Herbert", icpsrLegis := 15703]
committees_93102[name == "Lieberman, Joseph I.", icpsrLegis := 15704]
committees_93102[name == "Graham, Bob", icpsrLegis := 15503]
committees_93102[name == "Karnes, David K.", icpsrLegis := 15505]
committees_93102[name == "Sanford, Terry", icpsrLegis := 15504]
committees_93102[name == "Bingaman, Jeff", icpsrLegis := 14912]
committees_93102[name == "Bond, Christopher S.", icpsrLegis := 15501]
committees_93102[name == "Brady, Nicholas F.", icpsrLegis := 14911]
committees_93102[name == "Evans, Daniel J.", icpsrLegis := 14916]
committees_93102[name == "Hecht, Chic", icpsrLegis := 14913]
committees_93102[name == "Kerry, John F.", icpsrLegis := 14920]
committees_93102[name == "McCain, John", icpsrLegis := 15039]
committees_93102[name == "McConnell, Mitch", icpsrLegis := 14921]
committees_93102[name == "Robb, Charles S.", icpsrLegis := 15705]
committees_93102[name == "Rockefeller, John D. IV", icpsrLegis := 14922]
committees_93102[name == "Seymour, John", icpsrLegis := 49100]
committees_93102[name == "Wellstone, Paul", icpsrLegis := 49101]
committees_93102[name == "Burdick, Quentin N.", icpsrLegis := 1252]
committees_93102[name == "Burdick, Jocelyn", icpsrLegis := 49103]
committees_93102[name == "Wilson, Pete", icpsrLegis := 14915]
committees_93102[name == "Wofford, Harris", icpsrLegis := 49104]
committees_93102[name == "Smith, Robert C.", icpsrLegis := 15116]

setDT(committees_103112)
setnames(committees_103112, c("Congress", "ID..", "Committee.Code", "Name"),
  c("congress", "icpsrLegis", "code", "name"))
committees_103112[icpsrLegis == 914910, icpsrLegis := 94910] # Specter after
                                                             # party change
committees_103112[name == "Begich, Mark", icpsrLegis := 40900]
committees_103112[name == "Biden, Joseph R., Jr.", icpsrLegis := 14101]
committees_103112[name == "Blunt, Roy", icpsrLegis := 29735]
committees_103112[name == "Boozman, John", icpsrLegis := 20101]
committees_103112[name == "Burris, Roland", icpsrLegis := 40903]
committees_103112[name == "Campbell, Ben Nighthorse" & congress <= 103,
  icpsrLegis := 15407]
# four committee assignment record need to be duplicated for Campbell switch
committees_103112[name == "Campbell, Ben Nighthorse" & congress == 104,
  icpsrLegis := 15407]
x <- committees_103112[name == "Campbell, Ben Nighthorse" & congress == 104]
x[, icpsrLegis := 95407]
committees_103112 <- rbind(committees_103112, x)
committees_103112[name == "Campbell, Ben Nighthorse" & congress >= 105,
  icpsrLegis := 95407]
committees_103112[name == "Clinton, Hillary Rodham", icpsrLegis := 40105]
committees_103112[name == "Coons, Christopher", icpsrLegis := 40916]
committees_103112[name == "Franken, Al", icpsrLegis := 40904]
committees_103112[name == "Hagan, Kay", icpsrLegis := 40907]
committees_103112[name == "Inhofe, James M.", icpsrLegis := 15424]
committees_103112[name == "Jeffords, James M." & congress <= 106,
  icpsrLegis := 14240]
committees_103112[name == "Jeffords, James M." &
    Date.of.Termination == "6/6/2001",
  icpsrLegis := 14240]
# one committee assignment record needs to be duplicated, for Jeffords switch
committees_103112[name == "Jeffords, James M." &
    Date.of.Appointment == "1/25/2001" & Date.of.Termination == "1/3/2003",
  icpsrLegis := 14240]
x <- committees_103112[name == "Jeffords, James M." &
    Date.of.Appointment == "1/25/2001" & Date.of.Termination == "1/3/2003"]
x[, icpsrLegis := 94240]
committees_103112 <- rbind(committees_103112, x)
committees_103112[name == "Jeffords, James M." &
    Date.of.Appointment == "7/10/2001",
  icpsrLegis := 94240]
committees_103112[name == "Jeffords, James M." &
    Date.of.Appointment == "7/10/2001" & Date.of.Termination == "1/3/2003"]
committees_103112[name == "Jeffords, James M." & congress >= 108,
  icpsrLegis := 94240]
committees_103112[name == "Johanns, Mike", icpsrLegis := 40905]
committees_103112[name == "Kaufman, Ted", icpsrLegis := 40901]
committees_103112[name == "LeMieux, George", icpsrLegis := 40911]
committees_103112[name == "Manchin, Joe", icpsrLegis := 40915]
committees_103112[name == "Merkley, Jeff", icpsrLegis := 40908]
committees_103112[name == "Miller, Zell Bryan", icpsrLegis := 49904]
committees_103112[name == "Moran, Jerry", icpsrLegis := 29722]
committees_103112[name == "Portman, Rob", icpsrLegis := 29386]
committees_103112[name == "Reid, Harry", icpsrLegis := 15054]
committees_103112[name == "Reid, Harry M.", icpsrLegis := 15054]
committees_103112[name == "Risch, James", icpsrLegis := 40902]
committees_103112[name == "Shaheen, Jeanne", icpsrLegis := 40906]
committees_103112[name == "Bennett, Michael", icpsrLegis := 40910]
committees_103112[name == "Bennett, Michael", name := "Bennet, Michael"]
committees_103112[name == "Kirk, Mark", icpsrLegis := 20115]
committees_103112[name == "Kirk, Paul", icpsrLegis := 40912]
committees_103112[name == "Warner, Mark", icpsrLegis := 40909]
committees_103112 <- committees_103112[,
  .(congress, code, icpsrLegis, Senior.Party.Member, name)]

committees <- rbind(committees_93102, committees_103112)
committees <- committees[code <= 400 & congress >= 93]
committees[code == 304, code := 305]
committees[code == 313, code := 314]
committees[code == 320, code := 321]
committees[code == 328, code := 344]
committees[code == 342, code := 344]
committees[code == 348, code := 362]
committees[code == 352, code := 330]
committees[code == 354, code := 321]
committees[code == 363, code := 362]
committees[code == 374, code := 332]
committees[icpsrLegis == 2087 & congress <= 93,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 2087 & congress >= 94,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 2822 & congress <= 77,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 2822 & congress >= 78,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 3658 & congress <= 88,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 3658 & congress >= 91,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 4728 & congress <= 88,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 4728 & congress >= 92,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 14073 & congress <= 93 ,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 14073 & congress >= 95,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 14806 & congress <= 105,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 14806 & congress >= 112,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 14904 & congress <= 100,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 14904 & congress >= 101,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 14914 & congress <= 107,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 14914 & congress >= 108,
  icpsrLegis := 200000 + icpsrLegis]
committees[icpsrLegis == 15502 & congress <= 102,
  icpsrLegis := 100000 + icpsrLegis]
committees[icpsrLegis == 15502 & congress >= 103,
  icpsrLegis := 200000 + icpsrLegis]


committees <- rbind(
  merge(committees[congress >= 81 & congress <= 95],
    committee_values[min_cong == 81 & max_cong == 95, .(code, rank)],
    by = "code", all.x = TRUE),
  merge(committees[congress >= 96 & congress <= 103],
    committee_values[min_cong == 96 & max_cong == 103, .(code, rank)],
    by = "code", all.x = TRUE),
  merge(committees[congress >= 104 & congress <= 112],
    committee_values[min_cong == 104 & max_cong == 112, .(code, rank)],
    by = "code", all.x = TRUE))

best_committee_dt <- committees[congress >= 93,
  .(best_committee = min(rank, na.rm = TRUE),
    power_committee = as.numeric(length(intersect(rank, 1:4)) != 0)),
    .(congress, icpsrLegis, name)]

