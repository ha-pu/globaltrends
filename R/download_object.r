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
#' @param control Control batch that is used for mapping. Object of class
#' \code{numeric}. Defaults to \code{1}.
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
#' download_object(object = 1, locations = countries)
#' download_object(object = as.list(1:5), locations = countries)
#' }
#'
#' @export
#' @rdname download_object
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom stringr str_split

download_object <- function(object, control = 1, locations = countries) UseMethod("download_object", object)

#' @rdname download_object
#' @method download_object numeric
#' @export

download_object.numeric <- function(object, control = 1, locations = countries) {
  if (!is.character(locations)) stop(glue("Error: 'locations' must be object of type character.\nYou provided an element of type {typeof(locations)}."))
  if (length(object) > 1) {
    download_object(control = control, object = as.list(object), locations = locations)
  } else {
    .test_batch(object)
    terms_obj <- .keywords_object$keyword[.keywords_object$batch == object]
    time <- .time_object$time[.time_object$batch == object]

    walk(locations, ~ {
      if (.test_empty(table = "data_object", batch_o = object, batch_c = control, location = .x)) {
        qry_control <- filter(.tbl_control, batch == control & location == .x)
        qry_control <- collect(qry_control)
        if (nrow(qry_control) > 0) {
          terms_con <- summarise(group_by(qry_control, keyword), hits = mean(hits), .groups = "drop")
          terms_con <- terms_con$keyword[order(terms_con$hits)]
        }

        i <- 1
        while (i <= length(terms_con)) {
          out <- .get_trend(location = .x, term = c(terms_con[[i]], terms_obj), time = time)
          if (!is.null(out) & median(out$hits[out$keyword == terms_con[[i]]]) > 1) {
            out <- mutate(out, batch_c = control, batch_o = object)
            dbWriteTable(conn = globaltrends_db, name = "data_object", value = out, append = TRUE)
            break()
          }
          i <- i + 1
        }
        message(glue("Successfully downloaded object data | object: {object} | control: {control} | location: {.x} [{current}/{total}]",
          current = which(locations == .x), total = length(locations)
        ))
      }
    })
  }
}

#' @rdname download_object
#' @method download_object list
#' @export

download_object.list <- function(object, control = 1, locations = countries) {
  walk(object, download_object, control = control, locations = locations)
}
