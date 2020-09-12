#' @title Download data for mapping between control and object batch
#'
#' @description
#' @details
#'
#' @param control Control batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param lst_geo List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{gtrends_base}.
#'
#' @seealso
#'
#' @return Message that data was downloaded successfully. Data is uploaded
#' to data_map.
#'
#' @examples
#' \dontrun{
#' data_map(control = 1, object = 1, lst_geo = lst_wdi)
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date
#' @importFrom stats aggregate
#' @importFrom purrr walk
#' @importFrom stringr str_c

run_map <- function(control, object, lst_geo = lst_wdi) {
  walk(lst_geo, ~ {
    if (.test_empty(table = "data_map", batch_c = control, batch_o = object, geo = .x)) {
      qry_con <- filter(data_con, batch == control & geo == .x)
      qry_con <- collect(qry_con)
      qry_obj <- filter(data_obj, batch == object & geo == .x)
      qry_obj <- collect(qry_obj)
      if (nrow(qry_con) > 0 & nrow(qry_obj) > 0) {
        term_con <- summarise(group_by(qry_con, keyword), hits = mean(hits))
        term_con <- term_con$keyword[order(term_con$hits)]
        term_obj <- summarise(group_by(qry_obj, keyword), hits = mean(hits))
        term_obj <- term_obj$Group.1[term_obj$x == max(term_obj$x)]
        date_min <- as_date(max(min(qry_con$date), min(qry_obj$date)))
        date_max <- as_date(min(max(qry_con$date), max(qry_obj$date)))
        if (date_min < date_max) {
          i <- 1
          while (i <= 5) {
            out <- .get_trend(geo = .x, term = c(term_con[[i]], term_obj[[1]]), time = str_c(date_min, date_max, sep = " "))
            if (!is.null(out) & median(out$hits[out$keyword == term_con[[i]]]) > 1) {
              out <- mutate(out, batch_c = control, batch_o = object)
              dbWriteTable(conn = gtrends_db, name = "data_map", value = out, append = TRUE)
              break()
            }
            i <- i + 1
          }
        }
      }
    }
    message(str_c("Successfully downloaded mapping data | control: ", control, " | object: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
