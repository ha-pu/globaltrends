.compute_gini <- function (series) {
  out <- coalesce(1 - ineq::ineq(series, type = "Gini"), 0)
  return(out)
}

.compute_hhi <- function (series) {
  out <- coalesce(1 - sum((series / sum(series))^2), 0)
  return(out)
}

.compute_entropy <- function (series) {
  out <- coalesce(1 / ineq::ineq(series, parameter = 1, type = "entropy"), 0)
  if (out == Inf) {
    out <- 0
  }
  return(out)
}
