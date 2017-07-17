load("inst/extdata/raw_bios_extended.RData")
bios <- raw_bios
bios <- gsub("\u0092", "'", bios)
bios <- gsub("\u0093", '"', bios)
bios <- gsub("\u0094", '"', bios)
bios <- gsub("\\r\\n\\r\\nBibliography", "Bibliography: ", bios)
bios <- gsub("\\n", " ", bios)
bios <- gsub("\\r", " ", bios)
bios <- gsub("  +", " ", bios)
bios <- trimws(bios)
names(bios) <- senator_data$bioguide_id