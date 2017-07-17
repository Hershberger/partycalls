library(data.table)
files <- list.files("inst/extdata", full.names = TRUE)
files <- files[grep("pres", files)]
files <- lapply(files, scan, what = "raw", sep = "\n")
files <- lapply(files, function(x)
  paste(x[seq(1, length(x), 2)], x[seq(2, length(x), 2)], sep = ","))
ncols <- sapply(files, function(x) nchar(gsub("[^,]*", "", x[1])))
format_pres_votes <- function(x)
{
  out <- do.call(rbind, strsplit(x, ","))
  out <- as.data.table(out)
  ncols <- ncol(out)
  setnames(out, c("state", "total_votes", "dem_votes", "rep_votes",
    paste0("other_votes", seq_len(ncols - 4))))
  out
}
pres_votes <- lapply(files, format_pres_votes)
for (i in 1:length(pres_votes)) {
  pres_votes[[i]][, year := 1864 + 4 * i]
}

pres_votes <- rbindlist(pres_votes, fill = TRUE)
setcolorder(pres_votes,
  c("year", "state", "total_votes", "dem_votes", "rep_votes", "other_votes1",
  "other_votes2", "other_votes3"))
