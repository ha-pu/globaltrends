#' @title Reset date
#'
#' @keywords internal
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lubridate ymd

.reset_date <- function(data) {
  out <- mutate(data, day = 1, month = month(date), year = year(date))
  out <- group_by(out, location, keyword, year, month, day)
  out <- summarise(out, hits = mean(hits), .groups = "drop")
  out <- mutate(out, date = ymd(glue("{year}-{month}-{day}")))
  out <- select(out, location, keyword, date, hits)
  return(out)
}
