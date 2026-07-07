#' @title Package environment for internal state
#'
#' @description
#' `gt.env` is the internal package environment used to store runtime state and
#' data tables. It centralizes objects that should be shared across
#' functions (e.g., data.table stores, cached keyword batches).
#'
#' @details
#' The following bindings may be present in `gt.env` after package attach and/or
#' after calling initialization functions such as `start_db()`:
#' \itemize{
#'   \item `dt_keywords`: data.table of keyword batches (type, batch, keyword).
#'   \item `dt_time`: data.table of batch time windows (type, batch, start_date, end_date).
#'   \item `dt_control`: data.table of control search-volume data.
#'   \item `dt_object`: data.table of object search-volume data.
#'   \item `dt_score`: data.table of computed scores.
#'   \item `dt_doi`: data.table of DOI data.
#'   \item `dt_locations`: data.table of location set definitions.
#'   \item `dt_region`: data.table of regional search-volume data.
#'   \item `dt_related`: data.table of related search terms.
#'   \item `dt_synonyms`: data.table of keyword/synonym mappings.
#'   \item `keywords_control`: Cached data frame of control keywords by batch.
#'   \item `time_control`: Cached data frame of control batch time windows.
#'   \item `keywords_object`: Cached data frame of object keywords by batch.
#'   \item `time_object`: Cached data frame of object batch time windows.
#'   \item `keyword_synonyms`: Cached data frame of keyword/synonym mappings.
#'   \item `query_wait`: Numeric scalar. Seconds to wait between API calls (default: `0.1`).
#'   \item `py_setup`: Logical scalar. `TRUE` if [initialize_python()] has been called successfully.
#'   \item `api_calls`: Integer scalar. Number of Research API calls made today.
#'   \item `api_calls_date`: Date scalar. The date for which `api_calls` is counted.
#'   \item `score_calls`: Integer scalar. Number of locations processed by
#'     [compute_score()] since the database was last persisted.
#' }
#'
#' @format An environment with `parent = emptyenv()`.
#'
#' @seealso
#' * [start_db()]
#' * [initialize_python()]
#'
#' @export
gt.env <- new.env(parent = emptyenv())

# Declares this namespace as "data.table aware" to data.table's cedta()
# check. All data.table calls here are fully qualified (`data.table::`)
# rather than imported via NAMESPACE, so without this flag `[.data.table`
# silently downgrades to plain `[.data.frame` semantics whenever it's
# invoked from inside this package - both disabling binary-search joins
# (see `.get_full()`) and, more importantly, changing how bare symbols in
# `i`/`j` resolve. Every `dt[dt$col == localvar, ]`-style expression in this
# package has been audited so that no `localvar` shares a name with a
# column of `dt` (see `target_*` locals in remove_data.r, add_locations.r,
# and `.test_empty()`), since data.table's NSE would otherwise resolve such
# a bare symbol to the COLUMN rather than the local variable.
#' @keywords internal
#' @noRd
.datatable.aware <- TRUE

# These are referenced as bare data.table column names (NSE) in `[.data.table`
# calls (e.g. `.get_full()`'s `j` argument, and batch/type filters in
# aggregate_synonyms(), compute_score.numeric(), download_*.numeric(), and
# start_db()), which static analysis (R CMD check) cannot distinguish from
# undefined global variables. This is the standard data.table idiom for
# silencing that NOTE.
utils::globalVariables(c(
  "location", "batch", "batch_c", "batch_o", "locations", "rising", "topic", "type"
))

#' @keywords internal
#' @noRd
.onAttach <- function(libname, pkgname) {
  defaults <- list(
    dt_keywords  = NULL,
    dt_time      = NULL,
    dt_control   = NULL,
    dt_object    = NULL,
    dt_score     = NULL,
    dt_doi       = NULL,
    dt_locations = NULL,
    dt_region    = NULL,
    dt_related   = NULL,
    dt_synonyms  = NULL,
    keywords_control = NULL,
    time_control     = NULL,
    keywords_object  = NULL,
    time_object      = NULL,
    keyword_synonyms = NULL,
    query_wait   = 0.1,
    py_setup     = FALSE,
    api_calls    = 0L,
    api_calls_date = Sys.Date(),
    score_calls  = 0L
  )

  invisible(list2env(defaults, envir = gt.env))
}
