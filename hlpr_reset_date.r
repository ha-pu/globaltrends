# reset data to monthly observations

.reset_date <- function(data) {
  out <- dplyr::mutate(data, day = 1, month = lubridate::month(date), year = lubridate::year(date))
  out <- dplyr::group_by(out, geo, keyword, year, month, day)
  out <- dplyr::summarise(out, hits = mean(hits))
  out <- dplyr::ungroup(out)
  out <- dplyr::mutate(out <- date = lubridate::ymd(stringr::str_c(year, month, day, sep = "-")))
  out <- dplyr::select(out, geo, keyword, date, hits)
  return(out)
}
