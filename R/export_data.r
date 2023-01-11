#' @title Export data from database table
#'
#' @description
#' The function allows to export data from database tables. In combination with
#' various *write* functions in R, the functions allow exports from the
#' database to local files.
#'
#' @details
#' Exports can be filtered by *keyword*, *object*, *control*, *location*,
#' *locations*, or *type*. Not all filters are applicable for all
#' functions. When filter *keyword* and *object* are used together,
#' *keyword* overrules *object*. When supplying `NULL` as input, no filter is
#' applied to the variable.
#'
#' @param keyword Object keywords for which data should be exported. Object or
#' list of objects of type `character`.
#' @param object Object batch number for which data should be exported.
#' @param control Control batch number for which data should be exported. Only
#' for `export_control` and `export_control_global`, input is also possible as
#' list.
#' @param location List of locations for which the data is exported. Refers to
#' lists generated in `start_db` or `character` objects in these lists. Only for
#' `export_control`, `export_object`, or `export_score`
#' @param locations List of locations for which the data is exported. Refers to
#' names of lists generated in `start_db` as an object of type `character`. Only
#' for `export_doi`.
#' @param type Type of time series for which data should be exported. Element
#' of type `character`. Relevant only for `export_global` and
#' `export_doi`. Takes one of the following values: *obs* - observed
#' search scores, *sad* - seasonally adjusted search scores, *trd* -
#' trend only search scores.
#'
#' @return
#' The functions export and filter the respective database tables.
#' \itemize{
#'   \item `export_control` and `export_control_global` export data from table
#'   *data_control` with columns location, keyword, date, hits, control. Object
#'   of class `"data.frame"`. Methods are applied based on input *control*.
#'   \item `export_object` and `export_object_global` export data from table
#'   *data_object` with columns location, keyword, date, hits, object. Object of
#'   class `"data.frame"`. Methods are applied based on input *keyword*.
#'   \item `export_score` exports data from table *data_score` with
#'   columns location, keyword, date, score_obs, score_sad, score_trd, control,
#'   object. Object of class `c("exp_score", "data.frame")`. Methods are
#'   applied based on input *keyword*.
#'   \item `export_voi` exports data from table *data_score` with
#'   columns keyword, date, hits, control, filters for
#'   `location == "world"`. Object of class `c("exp_voi", "data.frame")`.
#'   Methods are applied based on input *keyword*.
#'   \item `export_doi` exports data from table *data_doi` with columns
#'   keyword, date, type, gini, hhi, entropy, control, object, locations. Object
#'   of class `c("exp_doi", "data.frame")`. Methods are applied based on input
#'   *keyword*.
#' }
#'
#' @seealso
#' * [example_control()]
#' * [example_object()]
#' * [example_score()]
#' * [example_doi()]
#'
#' @examples
#' \dontrun{
#' export_control(control = 2)
#'
#' export_object(
#'   keyword = "manchester united",
#'   locations = countries
#' )
#'
#' export_object(
#'   keyword = c("manchester united", "real madrid")
#' )
#'
#' export_object(
#'   keyword = list("manchester united", "real madrid")
#' )
#'
#' export_score(
#'   object = 3,
#'   control = 1,
#'   location = us_states
#' ) %>%
#'   readr::write_csv("data_score.csv")
#'
#' export_doi(
#'   keyword = "manchester united",
#'   control = 2,
#'   type = "sad",
#'   locations = "us_states"
#' ) %>%
#'   writexl::write_xlsx("data_doi.xlsx")
#' }
#' @rdname export_data
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom glue glue

export_control <- function(control = NULL, location = NULL) {
  if (!is.null(location)) location[location == "NA"] <- "NX" # handle namibia
  out <- .export_data(
    table = gt.env$tbl_control,
    in_batch = unlist(control),
    in_location = unlist(location)
  )
  out <- filter(out, .data$location != "world")
  out <- rename(out, control = .data$batch)
  out$location[out$location == "NX"] <- "NA" # handle namibia
  return(out)
}

#' @rdname export_data
#' @export

export_control_global <- function(control = NULL) {
  out <- .export_data(
    table = gt.env$tbl_control,
    in_batch = unlist(control),
    in_location = "world"
  )
  out <- rename(out, control = .data$batch)
  return(out)
}

#' @rdname export_data
#' @export

export_object <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  if (!is.null(location)) location[location == "NA"] <- "NX" # handle namibia
  out <- .export_data(
    table = gt.env$tbl_object,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = unlist(location)
  )
  out <- filter(out, .data$location != "world")
  out <- rename(out, object = .data$batch_o, control = .data$batch_c)
  out$location[out$location == "NX"] <- "NA" # handle namibia
  return(out)
}

#' @rdname export_data
#' @export

export_object_global <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data(
    table = gt.env$tbl_object,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = "world"
  )
  out <- rename(out, object = .data$batch_o, control = .data$batch_c)
  return(out)
}

#' @rdname export_data
#' @export

export_score <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  if (!is.null(location)) location[location == "NA"] <- "NX" # handle namibia
  out <- .export_data(
    table = gt.env$tbl_score,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = unlist(location),
  )
  out <- filter(out, .data$location != "world" & .data$synonym == 0)
  out <- rename(out, control = .data$batch_c, object = .data$batch_o)
  out <- select(out, -synonym)
  out$location[out$location == "NX"] <- "NA" # handle namibia
  class(out) <- c("exp_score", class(out))
  return(out)
}

#' @rdname export_data
#' @export

export_voi <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data(
    table = gt.env$tbl_score,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = "world"
  )
  out <- filter(out, .data$synonym == 0)
  out <- rename(out, control = .data$batch_c, object = .data$batch_o)
  out <- select(out, -synonym)
  class(out) <- c("exp_voi", class(out))
  return(out)
}

#' @rdname export_data
#' @export

export_doi <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = c("obs", "sad", "trd")) {
  type <- match.arg(type)
  out <- .export_data(
    table = gt.env$tbl_doi,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_locations = unlist(locations),
    in_type = unlist(type)
  )
  out <- rename(out, control = .data$batch_c, object = .data$batch_o)
  class(out) <- c("exp_doi", class(out))
  return(out)
}

#' @rdname dot-export_data
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date

.export_data <- function(table, in_keyword = NULL, in_object = NULL, in_control = NULL, in_batch = NULL, in_location = NULL, in_locations = NULL, in_type = NULL) {
  keyword <- in_keyword
  object <- in_object
  control <- in_control
  batch <- in_batch
  location <- in_location
  locations <- in_locations

  if (!is.null(in_keyword)) .check_input(keyword, "character")
  if (is.null(in_keyword) & !is.null(in_object)) .check_batch(in_object)
  if (!is.null(in_control)) .check_batch(in_control)
  if (!is.null(in_batch)) .check_batch(in_batch)
  if (!is.null(in_location)) .check_input(location, "character")
  if (!is.null(in_locations)) .check_input(locations, "character")

  if (!is.null(in_type)) in_type <- paste0("score_", in_type)
  if (!is.null(in_keyword)) table <- filter(table, .data$keyword %in% in_keyword)
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, .data$batch_o %in% in_object)
  if (!is.null(in_control)) table <- filter(table, .data$batch_c %in% in_control)
  if (!is.null(in_batch)) table <- filter(table, .data$batch == in_batch)
  if (!is.null(in_type)) table <- filter(table, .data$type %in% in_type)
  if (!is.null(in_location)) table <- filter(table, .data$location %in% in_location)
  if (!is.null(in_locations)) table <- filter(table, .data$locations %in% in_locations)

  table <- collect(table)
  table <- mutate(table, date = as_date(.data$date))
  return(table)
}
