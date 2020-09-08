#' @title Reset date
#' 
#' @keywords internal
#' 
#' @importFrom dpylr group_by
#' @importFrom dpylr mutate
#' @importFrom dpylr select
#' @importFrom dpylr summarise
#' @importFrom dpylr ungroup
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom dpylr lubridate ymd
#' @importFrom stringr str_c

.reset_date <- function(data) {
  out <- mutate(data, day = 1, month = month(date), year = year(date))
  out <- group_by(out, geo, keyword, year, month, day)
  out <- summarise(out, hits = mean(hits))
  out <- ungroup(out)
  out <- mutate(out, date = ymd(str_c(year, month, day, sep = "-")))
  out <- select(out, geo, keyword, date, hits)
  return(out)
}
