.reset_date <- function(data) {
  require(lubridate)
  out <- data %>%
    mutate(day = 1,
           month = month(date),
           year = year(date)) %>%
    group_by(geo, keyword, year, month, day) %>%
    summarise(hits = mean(hits)) %>%
    ungroup() %>%
    mutate(date = ymd(str_c(year, month, day, sep = "-"))) %>%
    select(-year, -month, -day)
  return(out)
}
