#' @title Export data from database tables
#'
#' @description
#' Seven functions for exporting filtered subsets of the four computed data
#' tables. Each function returns a data frame that can be passed directly to
#' standard R I/O functions such as `readr::write_csv()` or
#' `writexl::write_xlsx()`.
#'
#' | Function | Source table | Location scope |
#' |---|---|---|
#' | `export_control()` | `data_control` (control hits) | country/region level |
#' | `export_control_global()` | `data_control` | world aggregate only |
#' | `export_object()` | `data_object` (object hits) | country/region level |
#' | `export_object_global()` | `data_object` | world aggregate only |
#' | `export_score()` | `data_score` (normalized scores) | country/region level |
#' | `export_voi()` | `data_score` | world aggregate only (VOI) |
#' | `export_doi()` | `data_doi` (internationalization) | aggregated across locations |
#'
#' @details
#' All filter arguments default to `NULL`, which disables that filter and
#' returns all rows for that dimension. When `keyword` is provided it takes
#' precedence over `object`: the `object` argument is silently ignored.
#'
#' Non-`_global` functions (`export_control()`, `export_object()`,
#' `export_score()`) exclude the `"world"` aggregate row. The `_global`
#' counterparts (`export_control_global()`, `export_object_global()`,
#' `export_voi()`) return only the `"world"` row.
#'
#' @param keyword Character vector (or list coercible via `unlist()`) of object
#'   keywords to export. When provided, overrides `object`.
#'
#' @param object Integer scalar batch id for object data (`batch_o`). Ignored
#'   if `keyword` is supplied.
#'
#' @param control Integer scalar batch id for control data (`batch_c` or
#'   `batch`).
#'
#' @param location Character vector (or list coercible via `unlist()`) of
#'   location codes to filter by (e.g., values from [countries] or
#'   [us_states]).
#'
#' @param locations Character scalar naming a location set (e.g.,
#'   `"countries"`, `"us_states"`). Applies to `export_doi()` only.
#'
#' @return A data frame with the requested rows and a `date` column of class
#'   `Date`. Batch identifier columns are renamed for clarity:
#'
#'   - `export_control()`, `export_control_global()`: `location`, `keyword`,
#'     `date`, `hits`, `control` (renamed from `batch`).
#'   - `export_object()`, `export_object_global()`: `location`, `keyword`,
#'     `date`, `hits`, `object` (from `batch_o`), `control` (from `batch_c`).
#'   - `export_score()`, `export_voi()`: `location`, `keyword`, `date`,
#'     `score`, `control` (from `batch_c`), `object` (from `batch_o`).
#'   - `export_doi()`: `keyword`, `date`, `gini`, `hhi`, `entropy`,
#'     `control` (from `batch_c`), `object` (from `batch_o`), `locations`.
#'
#' @seealso
#' * [example_control], [example_object], [example_score], [example_doi]
#'   for the column structure of each table.
#' * [download_control()], [download_object()] to populate the source tables.
#' * [compute_score()], [compute_doi()] to compute scores and DOI metrics.
#' * [start_db()] to open a database session before exporting.
#'
#' @examples
#' \dontrun{
#' # Control hits for batch 2
#' export_control(control = 2)
#'
#' # World-aggregate control hits
#' export_control_global(control = 1)
#'
#' # Object hits for a keyword across all locations
#' export_object(keyword = "manchester united", location = countries)
#'
#' # Object hits for multiple keywords
#' export_object(keyword = c("manchester united", "real madrid"))
#'
#' # World-aggregate object hits
#' export_object_global(keyword = "manchester united", control = 1)
#'
#' # Location-level scores, written to CSV
#' export_score(object = 3, control = 1, location = us_states) |>
#'   readr::write_csv("data_score.csv")
#'
#' # Volume of interest (world-aggregate scores)
#' export_voi(keyword = "manchester united", control = 1)
#'
#' # Degree of internationalization for a keyword, written to Excel
#' export_doi(keyword = "manchester united", control = 2, locations = "us_states") |>
#'   writexl::write_xlsx("data_doi.xlsx")
#' }
#'
#' @rdname export_data
#' @export

export_control <- function(control = NULL, location = NULL) {
  out <- .export_data(
    table = "data_control",
    in_batch = control,
    in_location = location
  )
  out <- out[out$location != "world", , drop = FALSE]
  names(out)[names(out) == "batch"] <- "control"
  out
}

#' @rdname export_data
#' @export

export_control_global <- function(control = NULL) {
  out <- .export_data(
    table = "data_control",
    in_batch = control,
    in_location = "world"
  )
  names(out)[names(out) == "batch"] <- "control"
  out
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
    table = "data_object",
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = location
  )
  out <- out[out$location != "world", , drop = FALSE]
  names(out)[names(out) == "batch_o"] <- "object"
  names(out)[names(out) == "batch_c"] <- "control"
  out
}

#' @rdname export_data
#' @export

export_object_global <- function(
  keyword = NULL,
  object = NULL,
  control = NULL
) {
  out <- .export_data(
    table = "data_object",
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = "world"
  )
  names(out)[names(out) == "batch_o"] <- "object"
  names(out)[names(out) == "batch_c"] <- "control"
  out
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
    table = "data_score",
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = location
  )
  out <- out[out$location != "world", , drop = FALSE]
  names(out)[names(out) == "batch_c"] <- "control"
  names(out)[names(out) == "batch_o"] <- "object"
  out
}

#' @rdname export_data
#' @export

export_voi <- function(keyword = NULL, object = NULL, control = NULL) {
  out <- .export_data(
    table = "data_score",
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_location = "world"
  )
  names(out)[names(out) == "batch_c"] <- "control"
  names(out)[names(out) == "batch_o"] <- "object"
  out
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
    table = "data_doi",
    in_keyword = keyword,
    in_object = object,
    in_control = control,
    in_locations = locations
  )
  names(out)[names(out) == "batch_c"] <- "control"
  names(out)[names(out) == "batch_o"] <- "object"
  out
}

#' @description
#' Internal helper: build a SQL query from optional filters, execute it, and
#' convert the `date` column to class `Date`.
#'
#' All `in_*` arguments are coerced via `unlist()` before validation, so both
#' atomic vectors and lists are accepted from callers. Filters are applied only
#' when the corresponding argument is non-`NULL`. If `in_keyword` is supplied,
#' `in_object` is skipped entirely (keyword has precedence).
#'
#' @param table Character scalar. Name of the database table to query.
#' @param in_keyword Character vector of keywords; takes precedence over
#'   `in_object`.
#' @param in_object Integer scalar object batch id (`batch_o`). Ignored when
#'   `in_keyword` is supplied.
#' @param in_control Integer scalar control batch id (`batch_c`).
#' @param in_batch Integer scalar control batch id for control tables (`batch`
#'   column, not `batch_c`).
#' @param in_location Character vector of location codes.
#' @param in_locations Character scalar location-set name (DOI table only).
#'
#' @return A data frame with `date` converted to `Date`.
#'
#' @keywords internal
#' @noRd

.export_data <- function(
  table,
  in_keyword = NULL,
  in_object = NULL,
  in_control = NULL,
  in_batch = NULL,
  in_location = NULL,
  in_locations = NULL
) {
  in_keyword <- unlist(in_keyword)
  in_object <- unlist(in_object)
  in_control <- unlist(in_control)
  in_batch <- unlist(in_batch)
  in_location <- unlist(in_location)
  in_locations <- unlist(in_locations)

  # --- validate inputs --------------------------------------------------------
  if (!is.null(in_keyword)) .check_input(in_keyword, "character", name = "keyword")
  if (!is.null(in_control)) .check_batch(in_control)
  if (!is.null(in_batch)) .check_batch(in_batch)
  if (!is.null(in_location)) .check_input(in_location, "character")
  if (!is.null(in_locations)) .check_input(in_locations, "character")
  if (is.null(in_keyword) && !is.null(in_object)) .check_batch(in_object)

  con <- gt.env$globaltrends_db
  where_parts <- character(0)

  if (!is.null(in_keyword)) {
    kw_in <- paste(
      vapply(in_keyword, function(k) DBI::dbQuoteString(con, k), character(1)),
      collapse = ", "
    )
    where_parts <- c(where_parts, paste0("keyword IN (", kw_in, ")"))
  } else if (!is.null(in_object)) {
    where_parts <- c(where_parts, paste0("batch_o IN (", paste(in_object, collapse = ", "), ")"))
  }

  if (!is.null(in_control)) {
    where_parts <- c(where_parts, paste0("batch_c IN (", paste(in_control, collapse = ", "), ")"))
  }
  if (!is.null(in_batch)) {
    where_parts <- c(where_parts, paste0("batch IN (", paste(in_batch, collapse = ", "), ")"))
  }
  if (!is.null(in_location)) {
    loc_in <- paste(
      vapply(in_location, function(l) DBI::dbQuoteString(con, l), character(1)),
      collapse = ", "
    )
    where_parts <- c(where_parts, paste0("location IN (", loc_in, ")"))
  }
  if (!is.null(in_locations)) {
    locs_in <- paste(
      vapply(in_locations, function(l) DBI::dbQuoteString(con, l), character(1)),
      collapse = ", "
    )
    where_parts <- c(where_parts, paste0("locations IN (", locs_in, ")"))
  }

  sql <- paste0(
    "SELECT * FROM ", table,
    if (length(where_parts) > 0) paste0(" WHERE ", paste(where_parts, collapse = " AND ")) else ""
  )

  out <- DBI::dbGetQuery(con, sql)
  out$date <- as.Date(out$date)
  out
}
