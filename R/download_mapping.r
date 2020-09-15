#' @title Download data for mapping between control and object batch
#'
#' @aliases
#' download_mapping
#' download_mapping.numeric
#' download_mapping.list
#'
#' @description
#' @details
#'
#' @param control Control batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' elements of class \code{numeric}.
#' @param locations List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{start_db}.
#'
#' @seealso
#'
#' @return Message that data was downloaded successfully. Data is uploaded
#' to data_map.
#'
#' @examples
#' \dontrun{
#' data_map(control = 1, object = 1, locations = lst_wdi)
#' data_map(control = 1, object = as.list(1:5), locations = lst_wdi)
#' }
#'
#' @export
#' @rdname download_mapping
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom glue glue
#' @importFrom lubridate as_date
#' @importFrom purrr walk


download_mapping <- function(control, object, locations = lst_wdi) UseMethod("download_mapping", object)

#' @rdname download_mapping
#' @method download_mapping numeric
#' @export

download_mapping.numeric <- function(control, object, locations = lst_wdi) {
  walk(c(control, object), .test_batch)
  walk(locations, ~ {
    if (.test_empty(table = "data_map", batch_c = control, batch_o = object, geo = .x)) {
      qry_con <- filter(data_control, batch == control & geo == .x)
      qry_con <- collect(qry_con)
      qry_obj <- filter(data_object, batch == object & geo == .x)
      qry_obj <- collect(qry_obj)
      if (nrow(qry_con) > 0 & nrow(qry_obj) > 0) {
        term_con <- summarise(group_by(qry_con, keyword), hits = mean(hits))
        term_con <- term_con$keyword[order(term_con$hits)]
        term_obj <- summarise(group_by(qry_obj, keyword), hits = mean(hits))
        term_obj <- term_obj$keyword[term_obj$hits == max(term_obj$hits)]
        date_min <- as_date(max(min(qry_con$date), coalesce(min(qry_obj$date), min(qry_con$date))))
        date_max <- as_date(min(max(qry_con$date), coalesce(max(qry_obj$date), max(qry_con$date))))
        if (date_min < date_max) {
          i <- 1
          while (i <= length(term_con)) {
            out <- .get_trend(geo = .x, term = c(term_con[[i]], term_obj[[1]]), time = paste(date_min, date_max))
            if (!is.null(out) & median(out$hits[out$keyword == term_con[[i]]]) > 1) {
              out <- mutate(out, batch_c = control, batch_o = object)
              dbWriteTable(conn = doiGT_DB, name = "data_map", value = out, append = TRUE)
              break()
            }
            i <- i + 1
          }
        }
      }
    }
    message(glue("Successfully downloaded mapping data | control: {control} | object: {object} | geo: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
  })
}

#' @rdname download_mapping
#' @method download_mapping list
#' @export

download_mapping.list <- function(control, object, locations = lst_wdi) {
  walk(object, download_mapping, control = control[[1]], locations = locations)
}
