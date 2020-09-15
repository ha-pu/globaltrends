#' @title Export data from data_control table
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
#' element is used for exporting. Relevant only for "export_doi". Takes one
#' of the following values: "score_obs", "score_sad", "score_trd".
#' 
#' @seealso
#' 
#' @rdname export_data
#' @export

export_control <- function(control = NULL) {
  out <- .export_data(table = data_control, in_control = control)
  return(out)
}

#' @title Export data from data_object table
#' 
#' @rdname export_data
#' @export

export_object <- function(keyword = NULL, object = NULL) {
  out <- .export_data(table = data_object, in_keyword = keyword, in_object = object)
  return(out)

#' @title Export data from data_global table
#' 
#' @rdname export_data
#' @export

export_global <- function(keyword = NULL, object = NULL) {
  out <- .export_data(table = data_global, in_keyword = keyword, in_object = object)
  return(out)
}

#' @title Export data from data_mapping table
#' 
#' @rdname export_data
#' @export

export_mapping <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL) {
  out <- .export_data(table = data_mapping, in_keyword = keyword, in_object = object, in_control = control, in_locations = locations)
  return(out)
}

#' @title Export data from data_score table
#' 
#' @rdname export_data
#' @export

export_score <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL) {
  out <- .export_data(table = data_score, in_keyword = keyword, in_object = object, in_control = control, in_locations = locations)
  return(out)
}

#' @title Export data from data_doi table
#' 
#' @rdname export_data
#' @export

export_doi <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) {
  out <- .export_data(table = data_doi, in_keyword = keyword, in_object = object, in_control = control, in_locations = locations, in_type = type)
  return(out)
}

#' @title Export data from database tables
#' 
#' @keywords internal
#' 
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date

.export_data <- function(table, in_keyword = NULL, in_object = NULL, in_control = NULL, in_locations = NULL, in_type = NULL) {
  if (!is.null(in_keyword) & !is.character(in_keyword)) stop("Error: 'keyword' must be NULL or object of type character.\nYou supplied an object of a different type.")
  if (is.null(in_keyword) & !is.null(in_object)) .test_batch(in_object)
  if (!is.null(in_control)) .test_batch(in_control)
  if (!is.null(in_locations) & !is.character(in_locations)) stop("Error: 'locations' must be NULL or object of type character.\nYou supplied an object of a different type.")
  if (!is.null(in_type) & !is.character(in_type)) stop("Error: 'type' must be NULL or object of type character.\nYou supplied an object of a different type.")
  
  if (!is.null(in_keyword)) table <- filter(table, keyword == in_keyword[[1]])
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, object == in_object[[1]])
  if (!is.null(in_control)) table <- filter(table, control == in_control[[1]])
  if (!is.null(in_locations)) table <- filter(table, locations == in_locations[[1]])
  if (!is.null(in_type)) table <- filter(table, type == in_type[[1]])
  
  table <- collect(table)
  table <- mutate(table, date = as_date(date))
  return(table)
}
