#' @title Package environment for internal state
#'
#' @description
#' `gt.env` is the internal package environment used to store runtime state and
#' database handles. It centralizes objects that should be shared across
#' functions (e.g., the DBI connection, lazy table references, cached keyword
#' batches).
#'
#' @details
#' The following bindings may be present in `gt.env` after package attach and/or
#' after calling initialization functions such as `start_db()`:
#' \itemize{
#'   \item `globaltrends_db`: DBI connection/handle to the SQLite database.
#'   \item `tbl_locations`: Lazy table reference for location sets stored in the DB.
#'   \item `tbl_keywords`: Lazy table reference for keyword batches stored in the DB.
#'   \item `tbl_time`: Lazy table reference for time windows stored in the DB.
#'   \item `tbl_synonyms`: Lazy table reference for keyword synonyms stored in the DB.
#'   \item `tbl_doi`: Lazy table reference for DOI data stored in the DB.
#'   \item `tbl_control`: Lazy table reference for control search-volume data.
#'   \item `tbl_object`: Lazy table reference for object search-volume data.
#'   \item `tbl_score`: Lazy table reference for computed scores.
#'   \item `tbl_related`: Lazy table reference for related search terms.
#'   \item `tbl_region`: Lazy table reference for regional search-volume data.
#'   \item `keywords_control`: Cached tibble of control keywords by batch (populated by `start_db()` / exports).
#'   \item `time_control`: Cached tibble of control batch time windows.
#'   \item `keywords_object`: Cached tibble of object keywords by batch.
#'   \item `time_object`: Cached tibble of object batch time windows.
#'   \item `keyword_synonyms`: Cached tibble of keyword/synonym mappings.
#'   \item `query_wait`: Numeric scalar. Seconds to wait between API calls (default: `0.1`).
#'   \item `py_setup`: Logical scalar. `TRUE` if [initialize_python()] has been called successfully.
#'   \item `api_calls`: Integer scalar. Number of Research API calls made today (reset automatically at midnight).
#'   \item `api_calls_date`: Date scalar. The date for which `api_calls` is counted; used to detect day boundaries.
#' }
#'
#' @format An environment with `parent = emptyenv()`.
#'
#' @section Implementation notes:
#' The environment is created with `parent = emptyenv()` to avoid accidental
#' variable capture. Bindings are initialized on package attach so downstream
#' functions can rely on their existence; however, most bindings remain `NULL`
#' until `start_db()` (or related setup routines) populates them.
#'
#' @seealso
#' * [start_db()]
#' * [initialize_python()]
#' * [example_control()]
#' * [example_object()]
#' * [example_score()]
#' * [example_doi()]
#'
#' @export
gt.env <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  # Initialize expected bindings explicitly. Using NULL for most entries
  # makes "not yet configured" states unambiguous while ensuring names exist.
  defaults <- list(
    globaltrends_db = NULL,
    tbl_locations = NULL,
    tbl_keywords = NULL,
    tbl_time = NULL,
    tbl_synonyms = NULL,
    tbl_doi = NULL,
    tbl_control = NULL,
    tbl_object = NULL,
    tbl_score = NULL,
    tbl_related = NULL,
    tbl_region = NULL,
    keywords_control = NULL,
    time_control = NULL,
    keywords_object = NULL,
    time_object = NULL,
    keyword_synonyms = NULL,
    query_wait = 0.1,
    py_setup = FALSE,
    api_calls = 0L,
    api_calls_date = Sys.Date()
  )

  # Assign without copying into the search path; keep state contained in gt.env.
  invisible(list2env(defaults, envir = gt.env))
}
