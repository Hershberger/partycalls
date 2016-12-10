library(data.table)
library(magrittr)
library(rvest)
load("inst/extdata/legislators.RData")
raw_bios <- legislators %>%
  use_series(bioguide) %>%
  paste0("http://bioguide.congress.gov/scripts/biodisplay.pl?index=", .) %>%
  lapply(read_html) %>%
  lapply(html_text)
names(raw_bios) <- legislators %>% use_series(bioguide)
save(raw_bios, file = "inst/extdata/raw_bios_extended.RData")