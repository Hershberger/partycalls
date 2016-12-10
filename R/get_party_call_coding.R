
#' Get party call coding based on classifier results
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that was always a party call as a party call, any vote that was always
#' a noncall as a noncall, and the remainder as gray votes.
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return data.table with two columns, voteno and coding
#' @export
get_party_call_coding <- function(rc, n_iterations)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_always_classied_as_noncalls <- Reduce(intersect, record_of_coding)
  all_votes_ever_classied_as_noncalls <- Reduce(union, record_of_coding)
  gray_votes <- setdiff(all_votes_ever_classied_as_noncalls,
    all_votes_always_classied_as_noncalls)
  partycalls <- setdiff(seq_len(rc$m), all_votes_ever_classied_as_noncalls)
  noncalls <- all_votes_always_classied_as_noncalls
  voteno <- colnames(rc$votes)
  coding <- rep(NA, length(voteno))
  coding[gray_votes] <- "gray"
  coding[partycalls] <- "party call"
  coding[noncalls] <- "noncall"
  data.table(voteno, coding)
}

#' Retrieve party calls
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that was always a party call as a party call
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return character string of vote IDs, based on KH Data index
#' @export
get_party_calls <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_ever_classied_as_noncalls <- Reduce(union, record_of_coding)
  party_calls <- setdiff(seq_len(rc$m), all_votes_ever_classied_as_noncalls)
  paste("Vote", party_calls)
  colnames(rc$votes)[party_calls]
}

#' Retrieve noncalls
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that was always a noncall as a noncall.
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return character string of vote IDs, based on KH Data index
#' @export
get_noncalls <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_always_classied_as_noncalls <- Reduce(intersect, record_of_coding)
  noncalls <- all_votes_always_classied_as_noncalls
  colnames(rc$votes)[noncalls]
}

#' Retrieve gray votes
#'
#' Examines the last n_iterations of classifier iterations and codes any
#' vote that wasn't always a party call or always a noncall as a gray vote.
#' @param rc rollcall object, includes record_of_coding
#' @param n_iterations number of classifier iterations to include in coding
#' of roll call votes
#' @return character string of vote IDs, based on KH Data index
#' @export
get_gray_votes <- function(rc, n_iterations = 5)
{
  record_of_coding <- tail(rc$record_of_coding, n_iterations)
  all_votes_ever_classied_as_calls <- Reduce(union, record_of_coding)
  all_votes_always_classied_as_calls <- Reduce(intersect, record_of_coding)
  gray_votes <- setdiff(all_votes_ever_classied_as_calls,
    all_votes_always_classied_as_calls)
  colnames(rc$votes)[gray_votes]
}
