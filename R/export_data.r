#' @title Export data from database table
#'
#' @description
#' The function allows to export data from database tables.
#' 
#' @details
#' Exports can be filtered by *keyword*, *object*, *control*, *locations*, or
#' *type*. Not all filters are applicable for all functions. When filter
#' *keyword* and *object* are used together, *keyword* overrules *object*.
#' Currently the functions do not include list inputs - users are advised to
#' \code{purrr::map_dfr} or \code{dplyr::filter} instead.
#'
#' @param keyword Object keywords for which data should be exported. Object of
#' class \code{character}.
#' @param object Object batch number for which data should be exported.
#' @param control Control batch number for which data should be exported.
#' @param locations List of locations for which the search score is used.
#' For \code{export_control}, \code{export_object}, or \code{export_score}
#' refers to lists generated in \code{start_db}. For \code{export_doi}
#' object of class \code{character}.
#' @param type Type of time series for which data should be exported. Element
#' of class \code{character}. Relevant only for "export_global" and
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
#' \item \code{export_score} exports table  with columns location, keyword,
#' date, score_obs, score_sad, score_trd, control, object
#' \item \code{export_doi} exports table with columns keyword, date, type, gini,
#' hhi, entropy, control, object, locations
#' }
#'
#' @seealso \code{\link{data_control}}, \code{\link{data_object}},
#' \code{\link{data_score}}, \code{\link{data_doi}},
#' \code{\link[purrr]{map_dfr}}, \code{\link[dplyr]{filter}}
#'
#' @examples
#' \dontrun{
#' export_control(control = 2)
#'
#' export_object(keyword = "manchester united", locations = countries)
#'
#' export_score(object = 3, control = 1, locations = us_states) %>%
#'   readr::write_csv("data_score.csv")
#'
#' export_doi(
#'   keyword = "manchester united", control = 2, type = "sad",
#'   locations = "us_states"
#' ) %>%
#'   write_xl::write_xlsx("data_doi.xlsx")
#' }
#'
#' @rdname export_data
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom glue glue

export_control <- function(control = NULL, locations = NULL) {
  out <- .export_data_single(
    table = .tbl_control,
    in_control = control
  )
  if (!is.null(locations)) {
    in_location <- locations
    out <- filter(out, location %in% in_location)
  }
  out <- filter(out, location != "world")
  out <- rename(out, control = batch)
  return(out)
}

#' @rdname export_data
#' @export

export_control_global <- function(control = NULL) {
  out <- .export_data_single(
    table = .tbl_control,
    in_control = control
  )
  out <- filter(out, location == "world")
  out <- rename(out, control = batch)
  return(out)
}

#' @rdname export_data
#' @export

export_object <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL) {
  out <- .export_data_double(
    table = .tbl_object,
    in_keyword = keyword,
    in_object = object,
    in_control = control
  )
  if (!is.null(locations)) {
    in_location <- locations
    out <- filter(out, location %in% in_location)
  }
  out <- filter(out, location != "world")
  out <- rename(out, object = batch_o, control = batch_c)
  return(out)
}

#' @rdname export_data
#' @export

export_object_global <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data_double(
    table = .tbl_object,
    in_keyword = keyword,
    in_object = object,
    in_control = control
  )
  out <- filter(out, location == "world")
  out <- rename(out, object = batch_o, control = batch_c)
  return(out)
}

#' @rdname export_data
#' @export

export_score <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL) {
  out <- .export_data_double(
    table = .tbl_score,
    in_keyword = keyword,
    in_object = object,
    in_control = control
  )
  if (!is.null(locations)) {
    in_location <- locations
    out <- filter(out, location %in% in_location)
  }
  out <- filter(out, location != "world")
  out <- rename(out, control = batch_c, object = batch_o)
  out <- select(out, -synonym)
  return(out)
}

#' @rdname export_data
#' @export

export_voi <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data_double(
    table = .tbl_score,
    in_keyword = keyword,
    in_object = object,
    in_control = control
  )
  out <- filter(out, location == "world")
  out <- rename(out, control = batch_c, object = batch_o)
  out <- select(out, -synonym)
  return(out)
}

#' @rdname export_data
#' @export

export_doi <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) {
  out <- .export_data_double(
    table = .tbl_doi,
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_locations = locations,
    in_type = type
  )
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
  if (length(in_keyword) > 1) stop(glue("Error: 'keyword' must be object of length 1.\nYou provided an object of length {length(in_keyword)}."))
  if (length(in_object) > 1) stop(glue("Error: 'object' must be object of length 1.\nYou provided an object of length {length(in_object)}."))
  if (length(in_control) > 1) stop(glue("Error: 'control' must be object of length 1.\nYou provided an object of length {length(in_control)}."))
  if (length(in_type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(in_type)}."))
  in_keyword <- in_keyword[[1]]
  in_object <- in_object[[1]]
  in_control <- in_control[[1]]
  in_type <- in_type[[1]]

  if (!is.null(in_keyword) & !is.character(in_keyword)) stop(glue("Error: 'keyword' must be object of type character.\nYou supplied an object of type {typeof(in_keyword)}."))
  if (is.null(in_keyword) & !is.null(in_object)) .test_batch(in_object)
  if (!is.null(in_control)) .test_batch(in_control)
  if (!is.null(in_type)) if (!(in_type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {in_type}."))

  if (!is.null(in_type)) in_type <- paste0("hits_", in_type)
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
  if (length(in_keyword) > 1) stop(glue("Error: 'keyword' must be object of length 1.\nYou provided an object of length {length(in_keyword)}."))
  if (length(in_object) > 1) stop(glue("Error: 'object' must be object of length 1.\nYou provided an object of length {length(in_object)}."))
  if (length(in_control) > 1) stop(glue("Error: 'control' must be object of length 1.\nYou provided an object of length {length(in_control)}."))
  if (length(in_locations) > 1) stop(glue("Error: 'locations' must be object of length 1.\nYou provided an object of length {length(in_locations)}."))
  if (length(in_type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(in_type)}."))
  in_keyword <- in_keyword[[1]]
  in_object <- in_object[[1]]
  in_control <- in_control[[1]]
  in_locations <- in_locations[[1]]
  in_type <- in_type[[1]]

  if (!is.null(in_keyword) & !is.character(in_keyword)) stop(glue("Error: 'keyword' must be object of type character.\nYou supplied an object of type {typeof(in_keyword)}."))
  if (is.null(in_keyword) & !is.null(in_object)) .test_batch(in_object)
  if (!is.null(in_control)) .test_batch(in_control)
  if (!is.null(in_locations) & !is.character(in_locations)) stop(glue("Error: 'locations' must be object of type character.\nYou supplied an object of type {typeof(in_locations)}."))
  if (!is.null(in_type)) if (!(in_type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {in_type}."))

  if (!is.null(in_type)) in_type <- paste0("score_", in_type)
  if (!is.null(in_keyword)) table <- filter(table, keyword == in_keyword)
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, batch_o == in_object)
  if (!is.null(in_control)) table <- filter(table, batch_c == in_control)
  if (!is.null(in_locations)) table <- filter(table, locations == in_locations)
  if (!is.null(in_type)) table <- filter(table, type == in_type)

  table <- collect(table)
  table <- mutate(table, date = as_date(date))
  return(table)
}
