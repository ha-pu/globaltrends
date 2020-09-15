#' @title Download data for object batch
#'
#' @aliases
#' download_object
#' download_object.numeric
#' download_object.list
#'
#' @description
#' @details
#'
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' elements of class \code{numeric}.
#' @param locations List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{start_db}.
#'
#' @seealso
#'
#' @return
#' Message that data was downloaded successfully. Data is uploaded
#' to data_objectect.
#'
#' @examples
#' \dontrun{
#' data_object(object = 1, locations = lst_wdi)
#' data_object(object = as.list(1:5), locations = lst_wdi)
#' }
#'
#' @export
#' @rdname download_object
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom stringr str_split

download_object <- function(object, locations = lst_wdi) UseMethod("download_object", object)

#' @rdname download_object
#' @method download_object numeric
#' @export

download_object.numeric <- function(object, locations = lst_wdi) {
  .test_batch(object)
  terms <- terms_obj$keyword[terms_obj$batch == object]
  time <- time_obj$time[time_obj$batch == object]
  walk(locations, ~ {
    if (.test_empty(table = "data_object", batch_o = object, geo = .x)) {
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (is.null(out)) {
        start <- as_date(str_split(time, pattern = " ")[[1]][[1]])
        end <- as_date(str_split(time, pattern = " ")[[1]][[2]])
        out <- tibble(geo = .x, keyword = terms, hits = 0)
        out <- expand_grid(out, tibble(date = seq.Date(from = start, to = end, by = "month")))
      }
      out <- mutate(out, batch = object)
      dbWriteTable(conn = doiGT_DB, name = "data_object", value = out, append = TRUE)
    }
    message(glue("Successfully downloaded object data | object: {object} | geo: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
  })
}

#' @rdname download_object
#' @method download_object list
#' @export

download_object.list <- function(object, locations = lst_wdi) {
  walk(object, download_object, locations = locations)
}
