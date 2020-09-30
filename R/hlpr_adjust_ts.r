#' @title Adjust time series
#'
#' @keywords internal
#'
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom tibble tibble

.adjust_ts <- function(data) {
  myts <- stats::ts(data$hits, start = c(year(min(data$date)), month(min(data$date))), end = c(year(max(data$date)), month(max(data$date))), frequency = 12)
  fit <- stats::stl(myts, s.window = "period")
  trend <- fit$time.series[, "trend"]
  seasad <- forecast::seasadj(fit)
  out <- tibble(date = data$date, hits_obs = data$hits, hits_trd = as.double(trend), hits_sad = as.double(seasad))
  return(out)
}
