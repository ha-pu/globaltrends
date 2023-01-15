#' @title Download search trends from Google
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom gtrendsR gtrends
#' @importFrom lubridate as_date
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace

.get_trend <- function(location, term, time = "all", ...) {
  out <- try(gtrends(keyword = term, geo = location, time = time, onlyInterest = TRUE, ...))
  while (inherits(out, "try-error")) {
    if (str_detect(attr(out, "condition")$message, "Status code was not 200. Returned status code:500")) {
      message("globaltrends automatically retries download in 1s.")
      Sys.sleep(1)
    } else {
      message("globaltrends automatically retries download in 60s.")
      Sys.sleep(60)
    }
    out <- try(gtrends(keyword = term, geo = location, time = time, onlyInterest = TRUE))
  }
  if (is.null(out$interest_over_time)) {
    return(NULL)
  } else {
    out <- out$interest_over_time
    out <- mutate(out,
      hits = as.double(str_replace(.data$hits, "<1", "0.1")),
      date = as_date(.data$date)
    )
    out <- select(out, location = geo, keyword, date, hits)
    Sys.sleep(stats::runif(1, min = 5, max = 10))
    return(out)
  }
}

#' @title Test whether table has entries for given criteria
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter

.test_empty <- function(table, batch_c = NULL, batch_o = NULL, location = NULL, locations = NULL) {
  if (is.character(table)) {
    in_batch_c <- batch_c
    in_batch_o <- batch_o
    in_location <- location
    in_locations <- locations
    if (table == "data_control") {
      out <- filter(gt.env$tbl_control, .data$batch == in_batch_c & .data$location == in_location)
    } else if (table == "data_object") {
      out <- filter(gt.env$tbl_object, .data$batch_c == in_batch_c & .data$batch_o == in_batch_o & .data$location == in_location)
    } else if (table == "data_score") {
      out <- filter(gt.env$tbl_score, .data$batch_c == in_batch_c & .data$batch_o == in_batch_o & .data$location == in_location)
    } else if (table == "data_doi") {
      out <- filter(gt.env$tbl_doi, .data$batch_c == in_batch_c & .data$batch_o == in_batch_o & .data$locations == in_locations)
    } else if (table == "data_global") {
      out <- filter(gt.env$tbl_global, .data$batch == in_batch_o)
    }
    out <- utils::head(out)
    out <- collect(out)
    out <- nrow(out)
    out <- out == 0
    return(out)
  } else {
    stop("Error: 'table' must be an object of type character.\nYou provided an object of a different type.")
  }
}
