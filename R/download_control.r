#' @title Download data for control batch
#'
#' @aliases
#' download_control
#' download_control.numeric
#' download_control.list
#'
#' @description
#' @details
#'
#' @param control Control batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' elements of class \code{numeric}.
#' @param locations List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{start_db}.
#'
#' @seealso
#'
#' @return
#' Message that data was downloaded successfully. Data is uploaded
#' to data_control.
#'
#' @examples
#' \dontrun{
#' download_control(control = 1, locations = lst_wdi)
#' download_control(control = as.list(1:5), locations = lst_wdi)
#' }
#'
#' @export
#' @rdname download_control
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom purrr walk

download_control <- function(control, locations = lst_wdi) UseMethod("download_control", control)

#' @rdname download_control
#' @method download_control numeric
#' @export

download_control.numeric <- function(control, locations = lst_wdi) {
  .test_batch(control)
  terms <- keywords_control$keyword[keywords_control$batch == control]
  time <- time_control$time[time_control$batch == control]
  walk(locations, ~ {
    if (.test_empty(table = "data_control", batch_c = control, geo = .x)) {
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (!is.null(out)) {
        out <- mutate(out, batch = control)
        dbWriteTable(conn = doiGT_DB, name = "data_control", value = out, append = TRUE)
      }
    }
    message(glue("Successfully downloaded control data | control: {control} | geo: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
  })
}

#' @rdname download_control
#' @method download_control list
#' @export

download_control.list <- function(control, locations = lst_wdi) {
  walk(control, download_control, locations = locations)
}
