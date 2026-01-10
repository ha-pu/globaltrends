#' @title Export data from database tables
#'
#' @description
#' Export and filter data from the package's database-backed tables (control,
#' object, score/VOI, and DOI). The exported result can be written to disk using
#' standard R I/O functions (e.g., `readr::write_csv()`, `writexl::write_xlsx()`).
#'
#' @details
#' Each export function is a thin wrapper around an internal helper
#' (`.export_data()`) that applies optional filters. Passing `NULL` to a filter
#' argument disables that filter.
#'
#' Precedence rule: if `keyword` is provided, it takes precedence over `object`
#' (i.e., `object` is ignored when `keyword` is not `NULL`), matching the
#' behavior described in this documentation.
#'
#' @param keyword Character vector (or list coercible via `unlist()`) of object
#'   keywords to export. When provided, it overrides `object`.
#'
#' @param object Integer-ish batch id(s) for object data (`batch_o`). Ignored if
#'   `keyword` is supplied.
#'
#' @param control Integer-ish batch id(s) for control data (`batch_c` or `batch`).
#'
#' @param location Character vector (or list coercible via `unlist()`) of
#'   location codes to export (e.g., `countries`, `us_states`). Applicable to
#'   control/object/score exports.
#'
#' @param locations Character vector (or list coercible via `unlist()`) of
#'   location-set names (e.g., `"countries"`, `"us_states"`). Applicable to DOI.
#'
#' @return A `data.frame` (tibble) with the requested rows. Date columns are
#'   converted to `Date`.
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
#' export_object(keyword = "manchester united", location = countries)
#'
#' export_object(keyword = c("manchester united", "real madrid"))
#'
#' export_score(object = 3, control = 1, location = us_states) |>
#'   readr::write_csv("data_score.csv")
#'
#' export_doi(keyword = "manchester united", control = 2, locations = "us_states") |>
#'   writexl::write_xlsx("data_doi.xlsx")
#' }
#'
#' @rdname export_data
#' @export
#' @importFrom dplyr filter rename
#' @importFrom rlang .data

export_control <- function(control = NULL, location = NULL) {
  out <- .export_data(
    table = gt.env$tbl_control,
    in_batch = unlist(control),
    in_location = unlist(location)
  )

  # By convention, non-global exports exclude the aggregated "world" location.
  out <- filter(out, .data$location != "world")
  rename(out, control = .data$batch)
}

#' @rdname export_data
#' @export

export_control_global <- function(control = NULL) {
  out <- .export_data(
    table = gt.env$tbl_control,
    in_batch = unlist(control),
    in_location = "world"
  )
  rename(out, control = .data$batch)
}

#' @rdname export_data
#' @export

export_object <- function(
  keyword = NULL,
  object = NULL,
  control = NULL,
  location = NULL
) {
  out <- .export_data(
    table = gt.env$tbl_object,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = unlist(location)
  )

  out <- filter(out, .data$location != "world")
  rename(out, object = .data$batch_o, control = .data$batch_c)
}

#' @rdname export_data
#' @export

export_object_global <- function(
  keyword = NULL,
  object = NULL,
  control = NULL
) {
  out <- .export_data(
    table = gt.env$tbl_object,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = "world"
  )
  rename(out, object = .data$batch_o, control = .data$batch_c)
}

#' @rdname export_data
#' @export

export_score <- function(
  keyword = NULL,
  object = NULL,
  control = NULL,
  location = NULL
) {
  out <- .export_data(
    table = gt.env$tbl_score,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_location = unlist(location)
  )

  out <- filter(out, .data$location != "world")
  rename(out, control = .data$batch_c, object = .data$batch_o)
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
  rename(out, control = .data$batch_c, object = .data$batch_o)
}

#' @rdname export_data
#' @export

export_doi <- function(
  keyword = NULL,
  object = NULL,
  control = NULL,
  locations = NULL
) {
  out <- .export_data(
    table = gt.env$tbl_doi,
    in_keyword = unlist(keyword),
    in_object = unlist(object),
    in_control = unlist(control),
    in_locations = unlist(locations)
  )
  rename(out, control = .data$batch_c, object = .data$batch_o)
}

#' @title Export helper for database-backed tables
#'
#' @description
#' Internal helper that applies optional filters to a lazy database table,
#' collects the result, and normalizes the `date` column to class `Date`.
#'
#' @details
#' Filters are applied only when the corresponding `in_*` argument is not `NULL`.
#' If `in_keyword` is provided, it overrides `in_object` (object batch filtering
#' is skipped), mirroring the public API behavior.
#'
#' @param table A lazy table (typically a `dplyr` tbl backed by DBI/duckdb/sqlite).
#' @param in_keyword Optional character vector of keywords to filter by.
#' @param in_object Optional vector of object batch ids (`batch_o`). Ignored if
#'   `in_keyword` is supplied.
#' @param in_control Optional vector of control batch ids (`batch_c`).
#' @param in_batch Optional vector of control batch ids (`batch`) used in control tables.
#' @param in_location Optional character vector of location codes.
#' @param in_locations Optional character vector of location-set names (DOI only).
#'
#' @return A collected tibble.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter mutate
#' @importFrom lubridate as_date
#' @importFrom rlang .data

.export_data <- function(
  table,
  in_keyword = NULL,
  in_object = NULL,
  in_control = NULL,
  in_batch = NULL,
  in_location = NULL,
  in_locations = NULL
) {
  # --- validate inputs --------------------------------------------------------
  if (!is.null(in_keyword)) {
    .check_input(in_keyword, "character")
  }
  if (!is.null(in_control)) {
    .check_batch(in_control)
  }
  if (!is.null(in_batch)) {
    .check_batch(in_batch)
  }
  if (!is.null(in_location)) {
    .check_input(in_location, "character")
  }
  if (!is.null(in_locations)) {
    .check_input(in_locations, "character")
  }

  # Only validate `in_object` if it can actually be used (keyword has precedence).
  if (is.null(in_keyword) && !is.null(in_object)) {
    .check_batch(in_object)
  }

  # --- apply filters (on lazy table) -----------------------------------------
  if (!is.null(in_keyword)) {
    table <- filter(table, .data$keyword %in% in_keyword)
  } else if (!is.null(in_object)) {
    # Only applied if keyword is not supplied (precedence rule).
    table <- filter(table, .data$batch_o %in% in_object)
  }

  if (!is.null(in_control)) {
    table <- filter(table, .data$batch_c %in% in_control)
  }
  if (!is.null(in_batch)) {
    # Keep semantics: `in_batch` may be vector; `%in%` is robust and consistent.
    table <- filter(table, .data$batch %in% in_batch)
  }
  if (!is.null(in_location)) {
    table <- filter(table, .data$location %in% in_location)
  }
  if (!is.null(in_locations)) {
    table <- filter(table, .data$locations %in% in_locations)
  }

  # --- collect and normalize --------------------------------------------------
  table |>
    collect() |>
    mutate(date = as_date(.data$date))
}
