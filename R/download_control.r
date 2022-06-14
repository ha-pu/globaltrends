#' @title Download data for control keywords
#'
#' @aliases
#' download_control
#' download_control.numeric
#' download_control.list
#'
#' @description
#' The function downloads search volumes from Google Trends for a *control*
#' batch in a set of *locations*. Data is automatically written to table
#' *data_control*. For `download_control_global` the input
#' *location* is automatically set to *world*.
#'
#' @details
#' Downloads through the Google Trends API are made through
#' `gtrendsR::gtrends`. Each control batch can consist of up to five
#' keywords and is predefined in tables *batch_keywords* and
#' *batch_time* through `add_keywords`. The download for a single
#' keyword batch for a single location takes about 30 seconds. This includes a
#' randomized waiting period of 5-10 seconds between downloads. Depending on
#' the frequency of downloads, Google Trends might block users for some time. In
#' this case, `download_control` waits 60 minutes before it retries the
#' download.
#'
#' @param control Control batch for which the data is downloaded. Object
#' of type `numeric` or object of type `list` containing single
#' objects of type `numeric`.
#' @param locations List of countries or regions for which the data is
#' downloaded. Refers to lists generated in `start_db`. Defaults to
#' `countries`.
#' @param ... Arguments that are passed on to the `gtrendsR::gtrends` function.
#'
#' @seealso
#' * [example_control()]
#' * [gtrendsR::gtrends()]
#'
#' @return
#' Message that data has been downloaded successfully. Data is written
#' to table *data_control*.
#'
#' @examples
#' \dontrun{
#' download_control(
#'   control = 1,
#'   locations = countries
#' )
#' download_control(
#'   control = as.list(1:5),
#'   locations = countries
#' )
#' }
#'
#' @export
#' @rdname download_control
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom rlang .data

download_control <- function(control, locations = gt.env$countries, ...) UseMethod("download_control", control)

#' @rdname download_control
#' @method download_control numeric
#' @export

download_control.numeric <- function(control, locations = gt.env$countries, ...) {
  args <- list(...)
  .check_input(locations, "character")
  if (length(control) > 1) {
    download_control(control = as.list(control), locations = locations, ...)
  } else {
    .check_batch(control)
    terms <- gt.env$.keywords_control$keyword[gt.env$.keywords_control$batch == control]
    time <- gt.env$.time_control$time[gt.env$.time_control$batch == control]
    walk(locations, ~ {
      if (.x == "") {
        in_location <- "world"
      } else {
        in_location <- .x
      }
      if (.test_empty(table = "data_control", batch_c = control, location = in_location)) {
        out <- do.call(.get_trend, c(args, location = .x, term = list(terms), time = time))
        if (!is.null(out)) {
          out <- mutate(out, batch = control)
          dbWriteTable(conn = gt.env$globaltrends_db, name = "data_control", value = out, append = TRUE)
        }
        message(glue("Successfully downloaded control data | control: {control} | location: {in_location} [{current}/{total}]",
          current = which(locations == .x), total = length(locations)
        ))
      } else {
        message(glue("Control data already available | control: {control} | location: {in_location} [{current}/{total}]",
          current = which(locations == .x), total = length(locations)
        ))
      }
    })
  }
}

#' @rdname download_control
#' @method download_control list
#' @export

download_control.list <- function(control, locations = gt.env$countries, ...) {
  walk(control, download_control, locations = locations, ...)
}

#' @rdname download_control
#' @export

download_control_global <- function(control, ...) {
  download_control(control = control, locations = "", ...)
}
