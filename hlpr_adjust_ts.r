.adjust_ts <- function(data) {
  require(forecast)
  myts <- ts(data$hits, start = c(year(min(data$date)), month(min(data$date))), end = c(year(max(data$date)), month(max(data$date))), frequency = 12)
  fit <- stl(myts, s.window = "period")
  trend <- fit$time.series[,"trend"]
  seasad <- seasadj(fit)
  out <- tibble(date = data$date, hits_obs = data$hits, hits_trd = as.double(trend), hits_sad = as.double(seasad))
  return(out)
}
