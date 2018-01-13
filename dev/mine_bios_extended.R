# makes died_in_office, defeated_for_renomination, defeated_for_reelection,
#   changed_party_affiliation, resigned, did_not_seek_reelection
library(magrittr)
bios <- bios[match(senator_data$bioguide_id, names(bios))]
first_dash <- regexpr("-", bios)
bios_names <- trimws(substr(bios, 1, first_dash - 1))
year_born <- bios %>% regexpr(pattern = "\\([0-9]{4}") %>% add(1) %>%
  substr(x = bios, start = ., stop = . + 4) %>% trimws %>% as.integer
year_born[855] <- 1810
start <- bios %>% regexpr(pattern = "Senate Years of Service:") %>% add(24)
stop <- bios %>% regexpr(pattern = "Party:") %>% subtract(2)
years_of_service <- substr(bios, start, stop)
years_of_service <- gsub(",", ";", years_of_service)
multiple_terms <- which(sapply(years_of_service, function(x) {
  split <- strsplit(x, "") %>% unlist
  sum(split == "-")
  }) > 1)
years_of_service_multiple_terms <- strsplit(years_of_service[multiple_terms],
  ";")
ok <- order(sapply(years_of_service_multiple_terms, length))
multiple_terms <- multiple_terms[ok]
years_of_service_multiple_terms <- strsplit(years_of_service[multiple_terms],
  ";")


senator_data[multiple_terms, icpsrLegis][1:116]

bios["M001162"] <- gsub("Bibliography", "; Bibliography", bios["M001162"])
bios["O000167"] <- gsub("having been elected president",
  "; having been elected President", bios["O000167"])

deaths <- grep(paste0("to the (United States |U\\.S\\. )*Senate",
  "((?!resident)(?!unsuccessful)(?!resigned)",
  "(?!not a candidate for reelection).)*",
  "until (his|her) death"),
  bios, perl = TRUE)
deaths <- c(setdiff(deaths, c(351)), c(18, 220))
senator_data[, died_in_office := 0]
senator_data[deaths, died_in_office := 1]

defeated <- grep(paste0("to the (United States |U\\.S\\. )*Senate",
  ".*(defeated for renomination)|(unsuccessful candidate for renomination)"),
  bios, perl = TRUE)
defeated <- c(setdiff(defeated, c(192, 269, 357)), c(20))
senator_data[, defeated_for_renomination := 0]
senator_data[defeated, defeated_for_renomination := 1]

defeated <- grep(paste0("to the (United States |U\\.S\\. )*Senate",
  "((?!until (his|her) death)(?!did not seek reelection)",
  "(?!not a candidate for reelection).)*",
  "(unsuccessful (Democratic |Republican )*candidate for reelection)"),
  bios, perl = TRUE)
defeated <- c(setdiff(defeated, c(64, 138, 357)), c(50, 264, 311, 329))
senator_data[, defeated_for_reelection := 0]
senator_data[defeated, defeated_for_reelection := 1]

changed <- c(119, 160, 192, 211)
# bios_party <- substr(bios, regexpr("Party:", bios) + 7,
#   regexpr("Courtesy", bios) - 2)
# bios_party[grep(";|,", bios_party)]
# first_party <- substr(bios_party[grep(";|,", bios_party)], 1,
#   regexpr(";|,", bios_party[grep(";|,", bios_party)]) - 1)
# second_party <- substr(bios_party[grep(";|,", bios_party)],
#   regexpr(";|,", bios_party[grep(";|,", bios_party)]) + 2, 100)
# names(which(first_party != second_party))
# bios[names(which(first_party != second_party))]
senator_data[, changed_party_affiliation := 0]
senator_data[changed, changed_party_affiliation := 1]

resignees <- grep(paste0(
  "to the (United States |U\\.S\\. )Senate",
  "((?!until (his|her) death).)*",
  "((until (his|her) resignation)|(when (he|she) resigned))"),
  bios, perl = TRUE)
resignees <- c(setdiff(resignees, c(26, 348, 356, 357)), c())
senator_data[, resigned := 0]
senator_data[resignees, resigned := 1]

retirees <- grep(paste0("to (the )*(United States |U\\.S\\. )Senate",
  "((?!until (his|her) death)(?!unsuccessful)(?!resigned)",
  "(?!until (his|her) resignation).)*",
  "((not a candidate for (election|reelection|renomination|the six-year term))",
  "|(did not seek (election|reelection))",
  "|(withdrew from the race))",
  "|(BARKLEY, Dean)",# Dean Barkley was a retirement
  "|(KIRK, Paul)"),# Paul Kirk was a retirement
  bios, perl = TRUE)
retirees <- c(setdiff(retirees, c(53, 257, 348, 352, 358)),
  c(36, 40, 77, 86, 116, 225, 245, 351))
senator_data[, did_not_seek_reelection := 0]
senator_data[retirees, did_not_seek_reelection := 1]

# appointed <- grep("appointed", bios, perl = TRUE)
# appointed <- c(setdiff(appointed, c(357, 360)),
#   c())
# bios[appointed]
# appointed
#
# bios_split <- strsplit(bios, ";")
# X <- lapply(bios_split, function(x) {
#   pattern <- paste0("(to (the )*(United States |U\\.S\\. )*Senate)|",
#     "(become a U.S. Senator)")
#   start <- min(grep(pattern, x))
#   stop <- length(x)
#   x[start:stop]
#   x <- x[start:stop]
#   to_drop <- grep(paste0("Committee|member,|pleaded guilty|state senate|",
#     "interment|repose|lay in state|became governor|essayist|advisor|secretary|",
#     "Commission|ambassador|Ambassador|Secretary|entrepreneur|founder|envoy|",
#     "president|co-director|lecturer|practice of law|special counsel,|engaged|",
#     "continued|resumed|resident of|partner|chairman|denounced|delegate|",
#     "One Hundred|Ninety|Conference|whip|consultant|awarded|director|",
#     "Bibliography|Press|Palgrave|William Morrow|Kodansha|Viking|History|",
#     "Publishers|University|Printing|New York:|Books|Philadelphia:|Chicago:|",
#     "majority leader|minority leader|returned|fellow|died|resided|committee|",
#     "Autobiography|Washington, D.C.:|Biography|Washington:|Historical|edited|",
#     "Biographical|Scribner|lawyer|indicted|case dismissed|administrator|",
#     "Foreign Policy|Publishing|owner|United States federal judge|banker|",
#     "specialist|observer|CEO|Jefferson City, Mo.:|columnist"), x)
#   if (length(to_drop) == 0) {
#     x
#   } else {
#     x[-to_drop]
#   }
# })
#
# X <- sapply(X, paste, collapse = ";")
# X[grep("special", X)]
#
# which(sapply(bios_split, function(x) {
#   length(grep(paste0(#"Bibliography|until (his|her) death|is a resident of|",
#     "served from [A-Za-z0-9, ]*(until|to)[A-Za-z0-9, ]*"), x))
# })==0)
#
# independent <- c(60, 234, 342, 343)
#
# senator_data[did_not_seek_reelection == 0 & resigned == 0 &
#     died_in_office == 0 & defeated_for_reelection == 0 &
#     defeated_for_renomination == 0, .(mc, icpsrLegis)]
