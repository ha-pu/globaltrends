#' @title Download data for control batch
#'
#' @description
#' @details
#'
#' @param control Control batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param lst_geo List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{gtrends_base}.
#'
#' @seealso
#' 
#' @return
#' Message that data was downloaded successfully. Data is uploaded
#' to data_control.
#'
#' @examples
#' \dontrun{
#' data_con(control = 1, lst_geo = lst_wdi)
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stringr str_c

run_control <- function(control, lst_geo = lst_wdi) {
  terms <- terms_con$keyword[terms_con$batch == control]
  time <- time_con$time[time_con$batch == control]
  map(lst_geo, ~ {
    if (.test_empty(table = "data_con", batch_c = control, geo = .x)) {
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (!is.null(out)) {
        out <- mutate(out, batch = control)
        dbWriteTable(conn = gtrends_db, name = "data_con", value = out, append = TRUE)
      }
    }
    message(str_c("run_control | control: ", control, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
