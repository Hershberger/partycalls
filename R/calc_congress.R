#' Calculate congress corresponding to a year
#'
#' Simple conversion between year and congress
#' @param year integer year
#' @return integer congress
#' @export
calc_congress <- function(year)
{
  (year - 1788) / 2
}
