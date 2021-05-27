#' @title Export data from database table
#'
#' @description
#' The function allows to export data from database tables. In combination with
#' various *write* functions in R, the functions allow exports from the
#' database to local files.
#'
#' @details
#' Exports can be filtered by *keyword*, *object*, *control*,
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
#' @importFrom purrr map_dfr

export_control <- function(control = NULL, locations = NULL) UseMethod("export_control", control)

#' @rdname export_data
#' @method export_control numeric
#' @export

export_control.numeric <- function(control = NULL, location = NULL) {
  if (length(control) > 1) {
    out <- export_control(control = as.list(control), locations = locations)
  } else {
    out <- .export_data_single(
      table = .tbl_control,
      in_control = control,
      in_location = location
    )
    out <- filter(out, .data$location != "world")
    out <- rename(out, control = .data$batch)
  }
  return(out)
}

#' @rdname export_data
#' @method export_control NULL
#' @export

export_control.NULL <- function(control = NULL, location = NULL) {
  out <- .export_data_single(
    table = .tbl_control,
    in_control = control,
    in_location = location
  )
  out <- filter(out, .data$location != "world")
  out <- rename(out, control = .data$batch)
  return(out)
}

#' @rdname export_data
#' @method export_control list
#' @export

export_control.list <- function(control = NULL, location = NULL) {
  out <- map_dfr(control, export_control, location = location)
  return(out)
}

#' @rdname export_data
#' @export

export_control_global <- function(control = NULL) UseMethod("export_control_global", control)

#' @rdname export_data
#' @method export_control_global numeric
#' @export

export_control_global.numeric <- function(control = NULL) {
  if (length(control) > 1) {
    out <- export_control_global(control = as.list(control))
  } else {
    out <- .export_data_single(
      table = .tbl_control,
      in_control = control,
      in_location = "world"
    )
    out <- rename(out, control = .data$batch)
  }
  return(out)
}

#' @rdname export_data
#' @method export_control_global NULL
#' @export

export_control_global.NULL <- function(control = NULL) {
  out <- .export_data_single(
    table = .tbl_control,
    in_control = control,
    in_location = "world"
  )
  out <- rename(out, control = .data$batch)
  return(out)
}

#' @rdname export_data
#' @method export_control_global list
#' @export

export_control_global.list <- function(control = NULL) {
  out <- map_dfr(control, export_control_global)
  return(out)
}

#' @rdname export_data
#' @export

export_object <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL) UseMethod("export_object", keyword)

#' @rdname export_data
#' @method export_object character
#' @export

export_object.character <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  if (length(keyword) > 1) {
    out <- export_object(keyword = as.list(keyword), object = object, control = control, location = location)
  } else {
    out <- .export_data_double(
      table = .tbl_object,
      in_keyword = keyword,
      in_object = object,
      in_control = control,
      in_location = location
    )
    out <- filter(out, .data$location != "world")
    out <- rename(out, object = .data$batch_o, control = .data$batch_c)
  }
  return(out)
}

#' @rdname export_data
#' @method export_object NULL
#' @export

export_object.NULL <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  out <- .export_data_double(
    table = .tbl_object,
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = location
  )
  out <- filter(out, .data$location != "world")
  out <- rename(out, object = .data$batch_o, control = .data$batch_c)
  return(out)
}

#' @rdname export_data
#' @method export_object list
#' @export

export_object.list <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  out <- map_dfr(keyword, export_object, object = object, control = control, location = location)
  return(out)
}

#' @rdname export_data
#' @export

export_object_global <- function(keyword = NULL, object = NULL, control = NULL) UseMethod("export_object_global", keyword)

#' @rdname export_data
#' @method export_object_global character
#' @export

export_object_global.character <- function(keyword = NULL, object = NULL, control = NULL) {
  if (length(keyword) > 1) {
    out <- export_object_global(keyword = as.list(keyword), object = object, control = control)
  } else {
    out <- .export_data_double(
      table = .tbl_object,
      in_keyword = keyword,
      in_object = object,
      in_control = control,
      in_location = "world"
    )
    out <- rename(out, object = .data$batch_o, control = .data$batch_c)
  }
  return(out)
}

#' @rdname export_data
#' @method export_object_global NULL
#' @export

export_object_global.NULL <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data_double(
    table = .tbl_object,
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = "world"
  )
  out <- rename(out, object = .data$batch_o, control = .data$batch_c)
  return(out)
}

#' @rdname export_data
#' @method export_object list
#' @export

export_object_global.list <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- map_dfr(keyword, export_object_global, object = object, control = control)
  return(out)
}

#' @rdname export_data
#' @export

export_score <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) UseMethod("export_score", keyword)

#' @rdname export_data
#' @method export_score character
#' @export

export_score.character <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  if (length(keyword) > 1) {
    out <- export_score(keyword = as.list(keyword), object = object, control = control, location = location)
  } else {
    out <- .export_data_double(
      table = .tbl_score,
      in_keyword = keyword,
      in_object = object,
      in_control = control,
      in_location = location
    )
    out <- filter(out, .data$location != "world")
    out <- rename(out, control = .data$batch_c, object = .data$batch_o)
    out <- select(out, -.data$synonym)
    class(out) <- c("exp_score", class(out))
  }
  return(out)
}

#' @rdname export_data
#' @method export_score NULL
#' @export

export_score.NULL <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  out <- .export_data_double(
    table = .tbl_score,
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = location,
  )
  out <- filter(out, .data$location != "world")
  out <- rename(out, control = .data$batch_c, object = .data$batch_o)
  out <- select(out, -.data$synonym)
  class(out) <- c("exp_score", class(out))
  return(out)
}

#' @rdname export_data
#' @method export_score list
#' @export

export_score.list <- function(keyword = NULL, object = NULL, control = NULL, location = NULL) {
  out <- map_dfr(keyword, export_score, object = object, control = control, location = location)
  return(out)
}

#' @rdname export_data
#' @export

export_voi <- function(keyword = NULL, object = NULL, control = NULL) UseMethod("export_voi", keyword)

#' @rdname export_data
#' @method export_voi character
#' @export

export_voi.character <- function(keyword = NULL, object = NULL, control = NULL) {
  if (length(keyword) > 1) {
    out <- export_voi(keyword = as.list(keyword), object = object, control = control)
  } else {
    out <- .export_data_double(
      table = .tbl_score,
      in_keyword = keyword,
      in_object = object,
      in_control = control,
      in_location = "world"
    )
    out <- rename(out, control = .data$batch_c, object = .data$batch_o)
    out <- select(out, -.data$synonym)
    class(out) <- c("exp_voi", class(out))
  }
  return(out)
}

#' @rdname export_data
#' @method export_voi NULL
#' @export

export_voi.NULL <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data_double(
    table = .tbl_score,
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = "world"
  )
  out <- rename(out, control = .data$batch_c, object = .data$batch_o)
  out <- select(out, -.data$synonym)
  class(out) <- c("exp_voi", class(out))
  return(out)
}

#' @rdname export_data
#' @method export_voi list
#' @export

export_voi.list <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- map_dfr(keyword, export_voi, object = object, control = control)
  return(out)
}

#' @rdname export_data
#' @export

export_doi <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) UseMethod("export_doi", keyword)

#' @rdname export_data
#' @method export_doi character
#' @export

export_doi.character <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) {
  if (length(keyword) > 1) {
    out <- export_doi(keyword = as.list(keyword), object = object, control = control, locations = locations, type = type)
  } else {
    out <- .export_data_double(
      table = .tbl_doi,
      in_keyword = keyword,
      in_object = object,
      in_control = control,
      in_locations = locations,
      in_type = type
    )
    out <- rename(out, control = .data$batch_c, object = .data$batch_o)
    class(out) <- c("exp_doi", class(out))
  }
  return(out)
}

#' @rdname export_data
#' @method export_doi NULL
#' @export

export_doi.NULL <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) {
  out <- .export_data_double(
    table = .tbl_doi,
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_locations = locations,
    in_type = type
  )
  out <- rename(out, control = .data$batch_c, object = .data$batch_o)
  class(out) <- c("exp_doi", class(out))
  return(out)
}

#' @rdname export_data
#' @method export_doi list
#' @export

export_doi.list <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL) {
  out <- map_dfr(keyword, export_doi, object = object, control = control, locations = locations, type = type)
  return(out)
}


#' @title Run export data from database tables
#'
#' @rdname dot-export_data
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate as_date

.export_data_single <- function(table, in_keyword = NULL, in_object = NULL, in_control = NULL, in_type = NULL, in_location = NULL) {
  keyword <- in_keyword
  object <- in_object
  control <- in_control
  location <- in_location
  .check_length(keyword, 1)
  .check_length(object, 1)
  .check_length(control, 1)
  if (!is.null(in_type)) .check_type(in_type)

  if (!is.null(in_keyword)) .check_input(keyword, "character")
  if (is.null(in_keyword) & !is.null(in_object)) .check_batch(in_object)
  if (!is.null(in_control)) .check_batch(in_control)
  if (!is.null(in_location)) .check_input(location, "character")

  if (!is.null(in_type)) in_type <- paste0("hits_", in_type)
  if (!is.null(in_keyword)) table <- filter(table, .data$keyword == in_keyword)
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, .data$batch == in_object)
  if (!is.null(in_control)) table <- filter(table, .data$batch == in_control)
  if (!is.null(in_type)) table <- filter(table, .data$type == in_type)
  if (!is.null(in_location)) table <- filter(table, .data$location %in% in_location)

  table <- collect(table)
  table <- mutate(table, date = as_date(.data$date))
  return(table)
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

.export_data_double <- function(table, in_keyword = NULL, in_object = NULL, in_control = NULL, in_location = NULL, in_locations = NULL, in_type = NULL) {
  keyword <- in_keyword
  object <- in_object
  control <- in_control
  location <- in_location
  locations <- in_locations
  .check_length(keyword, 1)
  .check_length(object, 1)
  .check_length(control, 1)
  if (!is.null(in_type)) .check_type(in_type)

  if (!is.null(in_keyword)) .check_input(keyword, "character")
  if (is.null(in_keyword) & !is.null(in_object)) .check_batch(in_object)
  if (!is.null(in_control)) .check_batch(in_control)
  if (!is.null(in_location)) .check_input(location, "character")
  if (!is.null(in_locations)) .check_input(locations, "character")

  if (!is.null(in_type)) in_type <- paste0("score_", in_type)
  if (!is.null(in_keyword)) table <- filter(table, .data$keyword == in_keyword)
  if (is.null(in_keyword) & !is.null(in_object)) table <- filter(table, .data$batch_o == in_object)
  if (!is.null(in_control)) table <- filter(table, .data$batch_c == in_control)
  if (!is.null(in_type)) table <- filter(table, .data$type == in_type)
  if (!is.null(in_location)) table <- filter(table, .data$location %in% in_location)
  if (!is.null(in_locations)) table <- filter(table, .data$locations == in_locations)
  
  table <- collect(table)
  table <- mutate(table, date = as_date(.data$date))
  return(table)
}
