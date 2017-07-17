library(rvest)
url <- "http://uselectionatlas.org/RESULTS/data.php?year=1968&datatype=national&def=1&f=0&off=0&elect=0"

library(XML)
raw_xml <- readHTMLTable(url)

x <- read_xml(raw_html)
start <- regexpr("MapPieState", x) + 17
x <- substr(x, start, 1e6)
stop <- regexpr("EnableCustomize Table", x) - 1
x <- substr(x, 1, stop)
start <- regexpr("Alabama", x)
x <- substr(x, start, 1e6)




x <- gsub("[0-9]", ",", x)
x <- gsub("%", ",", x)
x <- gsub("\\s", ";", x)
x <- gsub("\\.", ",", x)
x <- substr(x, 101, 1e6)
x <- substr(x, 1, nchar(x) - 1)
x <- gsub(",,+", ",", x)
x <- gsub("D,C", "D. C.", x)

states <- c("Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "D. C.", "Florida", "Georgia", "Hawaii",
  "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
  "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan",
  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
  "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
  "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
  "West Virginia", "Wisconsin", "Wyoming")

x <- html_text(raw_html)[[1]]
starts <- sapply(states, regexpr, x, fixed = TRUE) + nchar(states)
stops <- c(tail(starts, -1) - 1,
  tail(starts, 1) + regexpr("Total", substr(x, tail(starts, 1), 1e6)) - 2)
sapply(seq_along(starts), function(i) substr(x, starts[i], stops[i]))
