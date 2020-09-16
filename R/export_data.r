#' @title Export data from database table
#'
#' @description
#' @details
#'
#' @param keyword Object keywords for which data should be exported. Element of
#' class \code{character}. When element length exceeds 1, only the first element
#' is used for exporting.
#' @param object Object batch number for which data should be exported. Element
#' of class \code{numeric}. When element length exceeds 1, only the first element
#' is used for exporting.
#' @param control Control batch number for which data should be exported. Element
#' of class \code{numeric}. When element length exceeds 1, only the first element
#' is used for exporting.
#' @param locations Set of locations for which data should be exported. Element
#' of class \code{character}. When element length exceeds 1, only the first
#' element is used for exporting.
#' @param type Type of timeseries for which data should be exported. Element
#' of class \code{character}. When element length exceeds 1, only the first
#' element is used for exporting. Relevant only for "export_global" and
#' "export_doi". Takes one of the following values: "obs", "sad", "trd".
#'
#' @return
#' The functions export and filter the respective database tables and return
#' objects of class \code{"tbl_df", "tbl", "data.frame"}.
#' \itemize{
#' \item \code{export_control} exports table with columns location, keyword,
#' date, hits, control
#' \item \code{export_object} exports table  with columns location, keyword,
#' date, hits, object
#' \item \code{export_global} exports table  with columns keyword, date, hits,
#' control
#' \item \code{export_mapping} exports table  with columns location, keyword,
#' date, hits, control, object
#' \item \code{export_score} exports table  with columns location, keyword,
#' date, score_obs, score_sad, score_trd, control, object
#' \item \code{export_doi} exports table with columns keyword, date, type, gini,
#' hhi, entropy, control, object, locations
#' }
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' export_control(control = 2)
#'
#' export_object(keyword = "manchester united")
#'
#' export_score(object = 3, control = 1) %>%
#'   readr::write_csv("data_score.csv")
#'
#' export_doi(keyword = "manchester united", control = 2, type = "sad",
#' locations = "us_states") %>%
#'   write_xl::write_xlsx("data_doi.xlsx")
#' }
#'
#' @rdname export_data
#' @export
#' @importFrom dplyr rename

export_control <- function(control = NULL) {
  out <- .export_data_single(table = data_control, in_control = control)
  out <- rename(out, control = batch)
  return(out)
}

#' @rdname export_data
#' @export

export_object <- function(keyword = NULL, object = NULL) {
  out <- .export_data_single(table = data_object, in_keyword = keyword, in_object = object)
  out <- rename(out, object = batch)
  return(out)
}

#' @rdname export_data
#' @export

export_global <- function(keyword = NULL, object = NULL, type = NULL) {
  out <- .export_data_single(table = data_global, in_keyword = keyword, in_object = object, in_type = type)
  out <- rename(out, object = batch)
  return(out)
}

#' @rdname export_data
#' @export

export_mapping <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data_double(table = data_mapping, in_keyword = keyword, in_object = object, in_control = control)
  out <- rename(out, control = batch_c, object = batch_o)
  return(out)
}

#' @rdname export_data
#' @export

export_score <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data_double(table = data_score, in_keyword = keyword, in_object = object, in_control = control)
  out <- rename(out, control = batch_c, object = batch_o)
  return(out)
}

#' @rdname export_data
#' @export

export_doi <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) {
  out <- .export_data_double(table = data_doi, in_keyword = keyword, in_object = object, in_control = control, in_locations = locations, in_type = type)
  out <- rename(out, control = batch_c, object = batch_o)
  return(out)
}

#' @title Run export data from database tables
#'
#' @rdname dot-export_data
#'
#' @keywords internal
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date

.export_data_single <- function(table, in_keyword = NULL, in_object = NULL, in_control = NULL, in_type = NULL) {
  in_keyword <- in_keyword[[1]]
  in_object <- in_object[[1]]
  in_control <- in_control[[1]]
  in_type <- in_type[[1]]
  if (!is.null(in_type)) in_type <- paste0("hits_", in_type)

  if (!is.null(in_keyword) & !is.character(in_keyword)) stop("Error: 'keyword' must be NULL or object of type character.\nYou supplied an object of a different type.")
  if (is.null(in_keyword) & !is.null(in_object)) .test_batch(in_object)
  if (!is.null(in_control)) .test_batch(in_control)
  if (!is.null(in_type) & !is.character(in_type)) stop("Error: 'type' must be NULL or object of type character.\nYou supplied an object of a different type.")

  if (!is.null(in_keyword)) table <- filter(table, keyword == in_keyword)
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, batch == in_object)
  if (!is.null(in_control)) table <- filter(table, batch == in_control)
  if (!is.null(in_type)) table <- filter(table, type == in_type)

  table <- collect(table)
  table <- mutate(table, date = as_date(date))
  return(table)
}

#' @rdname dot-export_data
#'
#' @keywords internal
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date

.export_data_double <- function(table, in_keyword = NULL, in_object = NULL, in_control = NULL, in_locations = NULL, in_type = NULL) {
  in_keyword <- in_keyword[[1]]
  in_object <- in_object[[1]]
  in_control <- in_control[[1]]
  in_locations <- in_locations[[1]]
  in_type <- in_type[[1]]
  if (!is.null(in_type)) in_type <- paste0("score_", in_type)

  if (!is.null(in_keyword) & !is.character(in_keyword)) stop("Error: 'keyword' must be NULL or object of type character.\nYou supplied an object of a different type.")
  if (is.null(in_keyword) & !is.null(in_object)) .test_batch(in_object)
  if (!is.null(in_control)) .test_batch(in_control)
  if (!is.null(in_locations) & !is.character(in_locations)) stop("Error: 'locations' must be NULL or object of type character.\nYou supplied an object of a different type.")
  if (!is.null(in_type) & !is.character(in_type)) stop("Error: 'type' must be NULL or object of type character.\nYou supplied an object of a different type.")

  if (!is.null(in_keyword)) table <- filter(table, keyword == in_keyword)
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, batch_o == in_object)
  if (!is.null(in_control)) table <- filter(table, batch_c == in_control)
  if (!is.null(in_locations)) table <- filter(table, locations == in_locations)
  if (!is.null(in_type)) table <- filter(table, type == in_type)

  table <- collect(table)
  table <- mutate(table, date = as_date(date))
  return(table)
}
