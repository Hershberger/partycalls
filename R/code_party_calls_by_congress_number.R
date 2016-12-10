#' TITLE
#'
#' DETAIL
#' @param congress_number integer values for the congress to analyze
#' @param chamber character string, house or senate
#' @param ... arguments to pass to \code{code_party_calls}
#' @export
#' @return xxx
code_party_calls_by_congress_number <- function(congress_number,
  chamber = "house", ...)
{
  cat("**** working on congress", congress_number, "\n")
  if (chamber == "house") {
    rc <- get(paste0("h", sprintf("%03.f", congress_number)))
  } else if (chamber == "senate") {
    rc <- get(paste0("sen", congress_number))
  }
  code_party_calls(rc, ...)
}
