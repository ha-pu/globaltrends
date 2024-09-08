#' @title Download search trends from Google
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom purrr map_dfr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @importFrom tibble tibble

.get_trend <- function(location, term, start_date = "2020-01", end_date = "2020-12") {
  if (exists("gt.env") == FALSE) {
      stop("Error: gtrends Python module not initialized. Please run 'initialize_python()' first.")
  }

  out <- gt.env$query_gtrends(
      terms = term,
      start_date = start_date,
      end_date = end_date,
      geo = location,
      api_key = gt.env$api_key
    )
  out <- map_dfr(
      out$lines,
      ~ {
        term <- .x$term
        values <- map_dbl(.x$points, ~ .x$value)
        dates <- map_chr(.x$points, ~ .x$date)
        dates <- as.Date(dates)
        out <- tibble(keyword = term, date = dates, hits = values)
        return(out)
      }
  )
  out$location <- location

  Sys.sleep(stats::runif(1, min = 5, max = 10))
  return(out)
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
