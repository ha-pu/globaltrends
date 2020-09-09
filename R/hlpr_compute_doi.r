#' @title Compute gini coefficient
#'
#' @keywords internal
#'
#' @importFrom dplyr coalesce
#' @importFrom ineq ineq

.compute_gini <- function(series) {
  out <- coalesce(1 - ineq(series, type = "Gini"), 0)
  return(out)
}

#' @title Compute herfindahl hirschman index
#'
#' @keywords internal

.compute_hhi <- function(series) {
  out <- coalesce(1 - sum((series / sum(series))^2), 0)
  return(out)
}

#' @title Compute entropy
#'
#' @keywords internal

.compute_entropy <- function(series) {
  out <- coalesce(1 / ineq(series, parameter = 1, type = "entropy"), 0)
  if (out == Inf) {
    out <- 0
  }
  return(out)
}
