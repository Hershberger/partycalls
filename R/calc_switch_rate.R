
#' TITLE
#'
#' DETAIL
#' @param rc xxx
#' @param DT xxx
#' @param tvals xxx
#' @param type xxx
#' @export
#' @return xxx
calc_switch_rate <- function(rc)
{
  record_of_coding <- rc$record_of_coding
  n_votes <- rc$m
  n <- length(record_of_coding)
  sapply(2:n, function(i)
    length(symdiff(record_of_coding[[i - 1]], record_of_coding[[i]]))) /
    n_votes
}
