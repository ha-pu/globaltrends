#' @title Download search trends from Google
#'
#' @keywords internal
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom gtrendsR gtrends
#' @importFrom lubridate as_date
#' @importFrom stringr str_replace

.get_trend <- function(location, term, time = "all") {
  out <- try(gtrends(keyword = term, geo = location, time = time, onlyInterest = TRUE))
  while (inherits(out, "try-error")) {
    if (attr(out, "condition")$message == "widget$status_code == 200 is not TRUE") {
      Sys.sleep(3600)
    } else {
      Sys.sleep(60)
    }
    out <- try(gtrends(keyword = term, geo = location, time = time, onlyInterest = TRUE))
  }
  if (is.null(out$interest_over_time)) {
    return(NULL)
  } else {
    out <- out$interest_over_time
    out <- mutate(out,
      hits = as.double(str_replace(hits, "<1", "0.1")),
      date = as_date(date)
    )
    out <- select(out, location = geo, keyword, date, hits)
    Sys.sleep(stats::runif(1, min = 20, max = 30))
    return(out)
  }
}
