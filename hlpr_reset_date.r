# reset data to monthly observations

.reset_date <- function(data) {
  out <- data %>%
    dplyr::mutate(day = 1,
           month = lubridate::month(date),
           year = lubridate::year(date)) %>%
    dplyr::group_by(geo, keyword, year, month, day) %>%
    dplyr::summarise(hits = mean(hits)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = lubridate::ymd(stringr::str_c(year, month, day, sep = "-"))) %>%
    dplyr::select(-year, -month, -day)
  return(out)
}
