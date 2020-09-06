# compute gini coefficient of distribution

.compute_gini <- function (series) {
  out <- dplyr::coalesce(1 - ineq::ineq(series, type = "Gini"), 0)
  return(out)
}

# compute herfindahl hirschman index of distribution

.compute_hhi <- function (series) {
  out <- dplyr::coalesce(1 - sum((series / sum(series))^2), 0)
  return(out)
}

# compute entropy of distribution

.compute_entropy <- function (series) {
  out <- dplyr::coalesce(1 / ineq::ineq(series, parameter = 1, type = "entropy"), 0)
  if (out == Inf) {
    out <- 0
  }
  return(out)
}