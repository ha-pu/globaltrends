# adjust time series

.adjust_ts <- function(data) {
  myts <- stats::ts(data$hits, start = c(lubridate::year(min(data$date)), lubridate::month(min(data$date))), end = c(lubridate::year(max(data$date)), lubridate::month(max(data$date))), frequency = 12)
  fit <- stats::stl(myts, s.window = "period")
  trend <- fit$time.series[,"trend"]
  seasad <- forecast::seasadj(fit)
  out <- tibble::tibble(date = data$date, hits_obs = data$hits, hits_trd = as.double(trend), hits_sad = as.double(seasad))
  return(out)
}
