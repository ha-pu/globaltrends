#' @title Download data for object batch
#'
#' @description
#' @details
#'
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric}.
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
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stringr str_c

run_object <- function(object, lst_geo = lst_wdi) {
  terms <- terms_obj$keyword[terms_obj$batch == object]
  time <- time_obj$time[time_obj$batch == object]
  x <- map(lst_geo, ~ {
    if (.test_empty(table = "data_obj", batch_o = object, geo = .x)) {
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (!is.null(out)) {
        out <- mutate(out, batch = object)
        dbWriteTable(conn = gtrends_db, name = "data_obj", value = out, append = TRUE)
      }
    }
    message(str_c("run_object | batch: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
