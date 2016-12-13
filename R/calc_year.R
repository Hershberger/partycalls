#' Calculate year corresponding to a congress
#'
#' Simple conversion between congress and year
#' @param congress integer congress
#' @return integer year
#' @export
calc_year <- function(congress)
{
  1788 + 2 * congress
}
