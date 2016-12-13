#' Calculate election year corresponding to a congress
#'
#' Calculate year of most previous regular election based on congress and class
#' class 1: up for reelection in 1964, 1970, 1976, 1982, 1988, 1994, 2000, 2006,
#'  2012; congresses in seq(94, 112, 3)
#' class 2: up for reelection in 1966, 1972, 1978, 1984, 1990, 1996, 2002, 2008,
#'  2014; congresses in seq(95, 113, 3)
#' class 3: up for reelection in 1968, 1974, 1980, 1986, 1992, 1998, 2004, 2010,
#'  2016; congresses in seq(93, 114, 3)
#' @param congress integer congress
#' @param class integer 1, 2, or 3; see Detail
#' @return integer year
#' @export
calc_year_elected <- function(congress, class)
{
  offset <- c(1, 0, 2)[class]
  1786 + 2 * (congress - ((congress + offset) %% 3))
}
