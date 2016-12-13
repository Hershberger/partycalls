get_legislator_yaml_i <- function(i, yaml_index, legislators_yaml, senator_data)
{
  yi <- yaml_index[i]
  ly <- legislators_yaml[[yi]]
  varnames <- c("url", "rss_url", "contact_form", "address", "phone", "fax",
    "office", "state_rank", "caucus", "party_affiliations")
  DTs <- lapply(ly$terms, function(lyt) {
    if ("party_affiliations" %in% names(lyt)) {
      lyt$party_affiliations <- paste(unlist(lyt$party_affiliations),
        collapse = ", ")
    }
    if ("address" %in% names(lyt)) {
      lyt$address <- paste(unlist(lyt$address), collapse = ", ")
    }
    DT_i <- as.data.table(lyt)
    if (DT_i$type[1] == "rep") {
      DT_i <- NULL
    } else {
      for (var in varnames) {
        if (!var %in% names(DT_i)) {
          DT_i[, VVV := NA]
          setnames(DT_i, "VVV", var)
        }
      }
      setcolorder(DT_i, c("type", "start", "end", "state", "class", "party",
        varnames))
    }
    DT_i
  })
  out <- rbindlist(DTs)
  out[, icpsrLegis := senator_data$icpsrLegis[i]]
  out[, yaml_index := yaml_index[i]]
  out
}

legislators_current_yaml <-
  yaml.load_file("inst/extdata/legislators-current.yaml")
legislators_historical_yaml <-
  yaml.load_file("inst/extdata/legislators-historical.yaml")
legislators_yaml <- c(legislators_historical_yaml, legislators_current_yaml)
bioguides <- sapply(legislators_yaml, function(y) y$id$bioguide)
yaml_index <- match(senator_data$bioguide_id, bioguides)
legislators_yaml <- rbindlist(lapply(seq_along(yaml_index),
  get_legislator_yaml_i, yaml_index, legislators_yaml, senator_data))
legislators_yaml[, `:=`(start = as.Date(start), end = as.Date(end))]

legislators_yaml <- legislators_yaml[, .(
  class = unique(class)[1]), icpsrLegis]
save(legislators_yaml, file = "inst/extdata/legislators_yaml.RData")
