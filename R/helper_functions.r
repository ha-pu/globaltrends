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
      hits = as.double(str_replace(.data$hits, "<1", "0.1")),
      date = as_date(.data$date)
    )
    out <- select(out, location = .data$geo, .data$keyword, .data$date, .data$hits)
    Sys.sleep(stats::runif(1, min = 20, max = 30))
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
      out <- filter(.tbl_control, .data$batch == in_batch_c & .data$location == in_location)
    } else if (table == "data_object") {
      out <- filter(.tbl_object, .data$batch_c == in_batch_c & .data$batch_o == in_batch_o & .data$location == in_location)
    } else if (table == "data_score") {
      out <- filter(.tbl_score, .data$batch_c == in_batch_c & .data$batch_o == in_batch_o & .data$location == in_location)
    } else if (table == "data_doi") {
      out <- filter(.tbl_doi, .data$batch_c == in_batch_c & .data$batch_o == in_batch_o & .data$locations == in_locations)
    } else if (table == "data_global") {
      out <- filter(.tbl_global, .data$batch == in_batch_o)
    }
    out <- utils::head(out)
    out <- collect(out)
    out <- nrow(out)
    out <- out == 0
    return(out)
  } else {
    stop("Error: 'table' must be an object of type character.\nYou supplied an object of a different type.")
  }
}
