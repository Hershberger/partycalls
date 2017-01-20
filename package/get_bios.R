# library(rvest)
# senator_data[, url := paste0(
#   "http://bioguide.congress.gov/scripts/biodisplay.pl?index=",
#   bioguide_id)]
# get_bio <- function(url)
# {
#   html_text(read_html(url))[[1]]
# }
# raw_bios <- pbapply::pblapply(senator_data$url, get_bio)
# save(raw_bios, file = "inst/extdata/raw_bios.RData")
load("inst/extdata/raw_bios.RData")
bios <- raw_bios
bios <- gsub("\u0092", "'", bios)
bios <- gsub("\u0093", '"', bios)
bios <- gsub("\u0094", '"', bios)
bios <- gsub("\\r\\n\\r\\nBibliography", "Bibliography: ", bios)
bios <- gsub("\\n", " ", bios)
bios <- gsub("\\r", " ", bios)
bios <- gsub("  +", " ", bios)
bios <- trimws(bios)
names(bios) <- unique(senator_data$bioguide_id)
#save(bios, file = "inst/extdata/bios.RData")
