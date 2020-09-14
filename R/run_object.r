#' @title Download data for object batch
#'
#' @aliases
#' run_object
#' run_object.numeric
#' run_object.list
#'
#' @description
#' @details
#'
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' elements of class \code{numeric}.
#' @param lst_geo List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{gtrends_base}.
#'
#' @seealso
#'
#' @return
#' Message that data was downloaded successfully. Data is uploaded
#' to data_object.
#'
#' @examples
#' \dontrun{
#' data_obj(object = 1, lst_geo = lst_wdi)
#' data_obj(object = as.list(1:5), lst_geo = lst_wdi)
#' }
#'
#' @export
#' @rdname run_object
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom stringr str_split

run_object <- function(object, lst_geo = lst_wdi) UseMethod("run_object", object)

#' @rdname run_object
#' @method run_object numeric
#' @export

run_object.numeric <- function(object, lst_geo = lst_wdi) {
  .test_batch(object)
  terms <- terms_obj$keyword[terms_obj$batch == object]
  time <- time_obj$time[time_obj$batch == object]
  walk(lst_geo, ~ {
    if (.test_empty(table = "data_obj", batch_o = object, geo = .x)) {
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (is.null(out)) {
        start <- as_date(str_split(time, pattern = " ")[[1]][[1]])
        end <- as_date(str_split(time, pattern = " ")[[1]][[2]])
        out <- tibble(geo = .x, keyword = terms, hits = 0)
        out <- expand_grid(out, tibble(date = seq.Date(from = start, to = end, by = "month")))
      }
      out <- mutate(out, batch = object)
      dbWriteTable(conn = gtrends_db, name = "data_obj", value = out, append = TRUE)
    }
    message(glue("Successfully downloaded object data | object: {object} | geo: {.x} [{current}/{total}]", current = which(lst_geo == .x), total = length(lst_geo)))
  })
}

#' @rdname run_object
#' @method run_object list
#' @export

run_object.list <- function(object, lst_geo = lst_wdi) {
  walk(object, run_object, lst_geo = lst_geo)
}
