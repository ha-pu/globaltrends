#' Initialize the local database store
#'
#' Creates the local database store used by `globaltrends` in the current
#' working directory and initializes all required tables and indexes.
#'
#' @details
#' The package uses DuckDB with a Parquet-backed persistence layout under the
#' `db/` folder. `initialize_db()` creates a transient in-memory DuckDB
#' database, builds the schema, populates default location sets, and exports
#' the result as Parquet files to `db/`. The in-memory connection is closed
#' before the function returns; call [start_db()] to open a working session.
#'
#' If all required Parquet files already exist the function returns early
#' without overwriting anything. If only some files are present (indicating a
#' partial or corrupted store) the function stops with an error.
#'
#' Default location sets written to `data_locations`:
#' \describe{
#'   \item{`countries`}{ISO 3166-1 alpha-2 codes for countries above the GDP
#'     share threshold (see [countries]).}
#'   \item{`us_states`}{ISO 3166-2 codes for US states and Washington DC
#'     (see [us_states]).}
#' }
#'
#' @section Concurrency:
#' DuckDB allows concurrent readers but has write constraints depending on the
#' underlying storage and process model. If you run parallel download workers,
#' use one database directory per worker and merge results afterwards.
#'
#' @return Invisibly returns `TRUE`. Called for its side effects (creating
#'   files under `db/`).
#'
#' @seealso [start_db()] to open a working session after initialization;
#'   [disconnect_db()] to persist changes and close the session.
#'
#' @examples
#' \dontrun{
#' initialize_db()
#' start_db()
#' }
#'
#' @export
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom duckdb duckdb
#' @importFrom purrr walk

initialize_db <- function() {
  .ensure_db_dir()

  check <- .check_files()
  if (isTRUE(all(check))) {
    message("Database files already exist under 'db/'.")
    return(invisible(TRUE))
  }

  # Schema definition (DuckDB SQL)
  schema_sql <- c(
    "CREATE TABLE batch_keywords(type VARCHAR, batch INTEGER, keyword VARCHAR);",
    "CREATE TABLE batch_time(type VARCHAR, batch INTEGER, start_date VARCHAR, end_date VARCHAR);",
    "CREATE TABLE data_control(location VARCHAR, keyword VARCHAR, date DATE, hits DOUBLE, batch INTEGER);",
    "CREATE TABLE data_object(location VARCHAR, keyword VARCHAR, date DATE, hits DOUBLE, batch_c INTEGER, batch_o INTEGER);",
    "CREATE TABLE data_score(location VARCHAR, keyword VARCHAR, date DATE, score DOUBLE, batch_c INTEGER, batch_o INTEGER);",
    "CREATE TABLE data_doi(keyword VARCHAR, date DATE, gini DOUBLE, hhi DOUBLE, entropy DOUBLE, batch_c INTEGER, batch_o INTEGER, locations VARCHAR);",
    "CREATE TABLE data_locations(location VARCHAR, type VARCHAR);",
    "CREATE TABLE data_region(term VARCHAR, location VARCHAR, start_date DATE, end_date DATE, region_code VARCHAR, region_name VARCHAR, hits DOUBLE, batch_o INTEGER);",
    "CREATE TABLE data_related(term VARCHAR, topic BOOLEAN, rising BOOLEAN, location VARCHAR, start_date DATE, end_date DATE, related_term VARCHAR, hits DOUBLE, batch_o INTEGER);",
    "CREATE TABLE keyword_synonyms(keyword VARCHAR, synonym VARCHAR);",
    "CREATE INDEX idx_doi_batch ON data_doi(batch_o);",
    "CREATE INDEX idx_control_batch ON data_control(batch);",
    "CREATE INDEX idx_locations_loc ON data_locations(location);",
    "CREATE INDEX idx_regions_term ON data_region(term);",
    "CREATE INDEX idx_regions_loc ON data_region(location);",
    "CREATE INDEX idx_related_term ON data_related(term);",
    "CREATE INDEX idx_object_batch ON data_object(batch_o);",
    "CREATE INDEX idx_score_batch ON data_score(batch_o);",
    "CREATE INDEX idx_terms_batch ON batch_keywords(batch);",
    "CREATE INDEX idx_time_batch ON batch_time(batch);"
  )

  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  walk(schema_sql, ~ dbExecute(con, .x))

  # Populate default location sets (writes into data_locations)
  .enter_location_defaults(con)

  # Persist database as Parquet under db/
  .export_db_to_parquet(con)

  message("Database files created successfully under 'db/'.")
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Internal helpers for DB filesystem layout
# -------------------------------------------------------------------------

#' @description Create `db/` directory if it does not yet exist.
#' @keywords internal
#' @noRd

.ensure_db_dir <- function(path = "db") {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(TRUE)
}

#' @description Return the canonical list of table/Parquet-file names.
#' @keywords internal
#' @noRd

.list_files <- function() {
  c(
    "batch_keywords",
    "batch_time",
    "data_control",
    "data_doi",
    "data_locations",
    "data_object",
    "data_score",
    "data_region",
    "data_related",
    "keyword_synonyms"
  )
}

#' @description
#' Check whether the required Parquet files exist under `path`. Returns a
#' logical vector (one element per file). Stops with an informative error if
#' only a subset of files is present, which indicates a partial or corrupted
#' store.
#' @keywords internal
#' @noRd

.check_files <- function(path = "db") {
  required <- paste0(.list_files(), ".parquet")
  present <- file.exists(file.path(path, required))

  if (any(present) && !all(present)) {
    stop(
      paste0(
        "Database files appear incomplete under '",
        path,
        "'. Missing file(s): ",
        paste(required[!present], collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  present
}

#' @description
#' Insert the package's built-in location sets (`countries`, `us_states`) into
#' the `data_locations` table of `con`. Called once by [initialize_db()].
#' @keywords internal
#' @noRd
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

.enter_location_defaults <- function(con) {
  data_to_add <- bind_rows(
    tibble(location = globaltrends::countries, type = "countries"),
    tibble(location = globaltrends::us_states, type = "us_states")
  )
  dbAppendTable(conn = con, name = "data_locations", value = data_to_add)
  invisible(TRUE)
}

#' @description
#' Export all tables in `con` as Parquet files to `path` using DuckDB's
#' `EXPORT DATABASE` statement, then remove the `load.sql` / `schema.sql`
#' helper files that DuckDB writes alongside the data files.
#' @keywords internal
#' @noRd

.export_db_to_parquet <- function(con, path = "db") {
  dbExecute(
    con,
    paste0(
      "EXPORT DATABASE '",
      path,
      "' (FORMAT parquet, USE_TMP_FILE false);"
    )
  )

  # DuckDB export creates helper SQL files; remove to keep db/ tidy
  helper_sql <- file.path(path, c("load.sql", "schema.sql"))
  suppressWarnings(file.remove(helper_sql))

  invisible(TRUE)
}

#' @description
#' Filter a lazy table to rows where `type == type_val`, drop the `type`
#' column, and collect the result into a local tibble. Used in [start_db()]
#' to populate the `keywords_*` and `time_*` caches.
#' @keywords internal
#' @noRd
#' @importFrom dplyr filter select collect
#' @importFrom rlang .data

.collect_by_type <- function(tbl, type_val) {
  tbl |>
    filter(.data$type == type_val) |>
    select(-.data$type) |>
    collect()
}

# -------------------------------------------------------------------------
# Start / Stop lifecycle
# -------------------------------------------------------------------------

#' Start a database session
#'
#' Loads the Parquet-backed store under `db/` into an in-memory DuckDB
#' connection and registers lazy `dplyr` table handles and cached tibbles in
#' `gt.env`.
#'
#' @details
#' Requires [initialize_db()] to have been run in the current working
#' directory. All Parquet files are read into an in-memory DuckDB instance;
#' the following bindings are written to `gt.env`:
#' \describe{
#'   \item{`globaltrends_db`}{Active `DBI` connection to the in-memory DuckDB
#'     instance.}
#'   \item{`tbl_locations`}{Lazy reference to `data_locations`.}
#'   \item{`tbl_keywords`}{Lazy reference to `batch_keywords`.}
#'   \item{`tbl_time`}{Lazy reference to `batch_time`.}
#'   \item{`tbl_synonyms`}{Lazy reference to `keyword_synonyms`.}
#'   \item{`tbl_doi`}{Lazy reference to `data_doi`.}
#'   \item{`tbl_control`}{Lazy reference to `data_control`.}
#'   \item{`tbl_object`}{Lazy reference to `data_object`.}
#'   \item{`tbl_score`}{Lazy reference to `data_score`.}
#'   \item{`tbl_region`}{Lazy reference to `data_region`.}
#'   \item{`tbl_related`}{Lazy reference to `data_related`.}
#'   \item{`keywords_control`, `keywords_object`}{Collected tibbles of control
#'     and object keywords by batch (without the `type` column).}
#'   \item{`time_control`, `time_object`}{Collected tibbles of batch time
#'     windows for control and object runs (without the `type` column).}
#'   \item{`keyword_synonyms`}{Collected tibble of all keyword/synonym pairs.}
#' }
#' Location sets are exported as named character vectors via
#' `.export_locations()`.
#'
#' @return Invisibly returns `TRUE`. Called primarily for its side effects.
#'
#' @seealso [initialize_db()] to create the store before the first session;
#'   [disconnect_db()] to persist changes and close the session.
#'
#' @examples
#' \dontrun{
#' start_db()
#' # ... downloads and computations ...
#' disconnect_db()
#' }
#'
#' @export
#' @importFrom DBI dbConnect dbExecute
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl collect filter select
#' @importFrom rlang .data
#' @importFrom purrr walk

start_db <- function() {
  check <- .check_files()
  if (!isTRUE(all(check))) {
    stop(
      "Database files do not exist under 'db/'. Run `initialize_db()` first.",
      call. = FALSE
    )
  }

  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  assign("globaltrends_db", con, envir = gt.env)

  # Import Parquet tables into the in-memory DB
  tables <- .list_files()
  import_sql <- paste0(
    "CREATE TABLE ", tables,
    " AS SELECT * FROM read_parquet('db/", tables, ".parquet');"
  )
  walk(import_sql, ~ dbExecute(con, .x))

  # Register lazy table handles
  tbl_locations <- tbl(con, "data_locations")
  tbl_keywords <- tbl(con, "batch_keywords")
  tbl_time <- tbl(con, "batch_time")
  tbl_synonyms <- tbl(con, "keyword_synonyms")

  tbl_doi <- tbl(con, "data_doi")
  tbl_control <- tbl(con, "data_control")
  tbl_object <- tbl(con, "data_object")
  tbl_score <- tbl(con, "data_score")
  tbl_region <- tbl(con, "data_region")
  tbl_related <- tbl(con, "data_related")

  # Cache small, frequently-used tables as in-memory tibbles
  keywords_control <- .collect_by_type(tbl_keywords, "control")
  keywords_object <- .collect_by_type(tbl_keywords, "object")
  time_control <- .collect_by_type(tbl_time, "control")
  time_object <- .collect_by_type(tbl_time, "object")
  keyword_synonyms <- tbl_synonyms |> collect()

  # Assign into gt.env
  lst_object <- list(
    globaltrends_db = con,
    tbl_locations = tbl_locations,
    tbl_keywords = tbl_keywords,
    tbl_time = tbl_time,
    tbl_synonyms = tbl_synonyms,
    tbl_doi = tbl_doi,
    tbl_control = tbl_control,
    tbl_object = tbl_object,
    tbl_score = tbl_score,
    tbl_region = tbl_region,
    tbl_related = tbl_related,
    keywords_control = keywords_control,
    time_control = time_control,
    keywords_object = keywords_object,
    time_object = time_object,
    keyword_synonyms = keyword_synonyms
  )
  invisible(list2env(lst_object, envir = gt.env))

  .export_locations()

  message("Successfully loaded database and exported table handles to gt.env.")
  invisible(TRUE)
}

#' Disconnect from the database and persist changes
#'
#' Exports the current in-memory DuckDB state to the Parquet store under
#' `db/` and closes the DBI connection.
#'
#' @details
#' Call this function after all downloads and computations are complete. It
#' overwrites the Parquet files under `db/` with the current in-memory state
#' and then shuts down the DuckDB instance. `gt.env$globaltrends_db` is set
#' to `NULL` afterwards; all lazy `tbl_*` handles become invalid.
#'
#' Data written to the in-memory database during the session will be **lost**
#' if this function is not called before the R session ends.
#'
#' @return Invisibly returns `TRUE`. Called for its side effects (writing
#'   files under `db/` and closing the connection).
#'
#' @seealso [initialize_db()] to create the store; [start_db()] to open a
#'   new session.
#'
#' @examples
#' \dontrun{
#' start_db()
#' # ... downloads and computations ...
#' disconnect_db()
#' }
#'
#' @export
#' @importFrom DBI dbDisconnect

disconnect_db <- function() {
  if (is.null(gt.env$globaltrends_db)) {
    stop(
      "No active database connection found in `gt.env$globaltrends_db`.",
      call. = FALSE
    )
  }

  .export_db_to_parquet(gt.env$globaltrends_db)

  dbDisconnect(conn = gt.env$globaltrends_db, shutdown = TRUE)
  assign("globaltrends_db", NULL, envir = gt.env)

  message("Successfully disconnected and persisted database to 'db/'.")
  invisible(TRUE)
}
