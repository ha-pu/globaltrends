#' @title Download data for object batch
#'
#' @aliases
#' download_object
#' download_object.numeric
#' download_object.list
#'
#' @description
#' The function downloads search volumes from Google Trends for an object batch
#' (\emph{batch_o}) and one keyword from a control batch (\emph{batch_c}) in a
#' set of \emph{locations}. Data is automatically written to table
#' \emph{data_object}. For \code{download_object_global} the input
#' \emph{location} is automatically set to \emph{world}.
#'
#' @details
#' Downloads through the Google Trends API are made through
#' \code{gtrendsR::gtrends}. Each object batch can consist of up to four
#' keywords and is predefined in tables \emph{batch_keywords} and
#' \emph{batch_time} through \code{add_keywords}. In addition, one control
#' keyword is added to each object batch. The control keyword then allows a
#' mapping between search volumes for control keywords stored in
#' \emph{data_control} and search volumes for object keywords. The download for
#' a single keyword batch for a single location takes about 30 seconds. This
#' includes a randomized waiting period of 20-30 seconds between downloads.
#' Depending on the frequency of downloads, Google Trends might block users
#' for some time. In this case, \code{download_object} waits 60 minutes
#' before it retries the download.
#'
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' object of class \code{numeric}.
#' @param control Control batch that is used for mapping. Object of class
#' \code{numeric}. Defaults to \code{1}.
#' @param locations List of countries or regions for which the data is
#' downloaded. Refers to lists generated in \code{start_db}. Defaults to
#' \code{countries}.
#'
#' @seealso
#' * \code{\link{data_object}}
#' * \code{\link[gtrendsR]{gtrends}}
#'
#' @return
#' Message that data was downloaded successfully. Data is written
#' to table \emph{data_object}.
#'
#' @examples
#' \dontrun{
#' download_object(
#'   object = 1,
#'   locations = countries
#' )
#' download_object(
#'   object = as.list(1:5),
#'   locations = countries
#' )
#' }
#'
#' @export
#' @rdname download_object
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom rlang .data

download_object <- function(object, control = 1, locations = countries) UseMethod("download_object", object)

#' @rdname download_object
#' @method download_object numeric
#' @export

download_object.numeric <- function(object, control = 1, locations = countries) {
  if (length(control) > 1) stop(glue("Error: 'control' must be object of length 1.\nYou provided an object of length {length(control)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be object of type character.\nYou provided an object of type {typeof(locations)}."))
  if (length(object) > 1) {
    download_object(control = control, object = as.list(object), locations = locations)
  } else {
    walk(list(control, object), .test_batch)
    terms_obj <- .keywords_object$keyword[.keywords_object$batch == object]
    time <- .time_object$time[.time_object$batch == object]

    walk(locations, ~ {
      if (.x == "") {
        in_location <- "world"
      } else {
        in_location <- .x
      }
      if (.test_empty(table = "data_object", batch_o = object, batch_c = control, location = in_location)) {
        qry_control <- filter(.tbl_control, .data$batch == control & .data$location == in_location)
        qry_control <- collect(qry_control)
        if (nrow(qry_control) > 0) {
          terms_con <- summarise(
            group_by(qry_control, .data$keyword),
            hits = mean(.data$hits),
            .groups = "drop"
          )
          terms_con <- terms_con$keyword[order(terms_con$hits)]

          i <- 1
          while (i <= length(terms_con)) {
            out <- .get_trend(location = .x, term = c(terms_con[[i]], terms_obj), time = time)
            if (!is.null(out) & median(out$hits[out$keyword == terms_con[[i]]]) > 1) {
              out <- mutate(
                out,
                batch_c = control,
                batch_o = object
              )
              dbWriteTable(conn = globaltrends_db, name = "data_object", value = out, append = TRUE)
              break()
            }
            i <- i + 1
          }
          message(glue("Successfully downloaded object data | object: {object} | control: {control} | location: {in_location} [{current}/{total}]",
            current = which(locations == .x), total = length(locations)
          ))
        } else {
          message(glue("Download for objec data failed.\nThere is no data in 'data_control' for control batch {control} and location {in_location}."))
        }
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

#' @rdname download_object
#' @export

download_object_global <- function(object, control = 1) {
  download_object(object = object, control = control, locations = "")
}
