#' Initialize the local database store
#'
#' Creates the local database store used by `globaltrends` in the current
#' working directory and initializes all required tables and indexes.
#'
#' @details
#' The package uses SQLite with a Parquet-backed persistence layout under the
#' `db/` folder. `initialize_db()` creates a transient in-memory SQLite
#' database, builds the schema, populates default location sets, and exports
#' the result as Parquet files (via `arrow`) to `db/`. The in-memory connection
#' is closed before the function returns; call [start_db()] to open a working
#' session.
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
#' SQLite allows concurrent readers but only one writer at a time. If you run
#' parallel download workers, use one database directory per worker and merge
#' results afterwards.
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
#' @importFrom RSQLite SQLite

initialize_db <- function() {
  .ensure_db_dir()

  check <- .check_files()
  if (isTRUE(all(check))) {
    message("Database files already exist under 'db/'.")
    return(invisible(TRUE))
  }

  # Schema definition (DuckDB SQL)
  schema_sql <- c(
    "CREATE TABLE batch_keywords(type TEXT, batch INTEGER, keyword TEXT);",
    "CREATE TABLE batch_time(type TEXT, batch INTEGER, start_date TEXT, end_date TEXT);",
    "CREATE TABLE data_control(location TEXT, keyword TEXT, date DATE, hits REAL, batch INTEGER);",
    "CREATE TABLE data_object(location TEXT, keyword TEXT, date DATE, hits REAL, batch_c INTEGER, batch_o INTEGER);",
    "CREATE TABLE data_score(location TEXT, keyword TEXT, date DATE, score REAL, batch_c INTEGER, batch_o INTEGER);",
    "CREATE TABLE data_doi(keyword TEXT, date DATE, gini REAL, hhi REAL, entropy REAL, batch_c INTEGER, batch_o INTEGER, locations TEXT);",
    "CREATE TABLE data_locations(location TEXT, type TEXT);",
    "CREATE TABLE data_region(term TEXT, location TEXT, start_date DATE, end_date DATE, region_code TEXT, region_name TEXT, hits REAL, batch_o INTEGER);",
    "CREATE TABLE data_related(term TEXT, topic INTEGER, rising INTEGER, location TEXT, start_date DATE, end_date DATE, related_term TEXT, hits REAL, batch_o INTEGER);",
    "CREATE TABLE keyword_synonyms(keyword TEXT, synonym TEXT);",
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

  con <- dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  on.exit(dbDisconnect(con), add = TRUE)

  for (sql in schema_sql) dbExecute(con, sql)

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

.enter_location_defaults <- function(con) {
  data_to_add <- rbind(
    data.frame(location = globaltrends::countries, type = "countries", stringsAsFactors = FALSE),
    data.frame(location = globaltrends::us_states, type = "us_states", stringsAsFactors = FALSE)
  )
  dbAppendTable(conn = con, name = "data_locations", value = data_to_add)
  invisible(TRUE)
}

#' @description
#' Export all tables in `con` as Parquet files to `path` using
#' `arrow::write_parquet()`.
#' @keywords internal
#' @noRd

.export_db_to_parquet <- function(con, path = "db") {
  tables <- .list_files()
  for (tbl_name in tables) {
    df <- DBI::dbReadTable(con, tbl_name)
    arrow::write_parquet(df, file.path(path, paste0(tbl_name, ".parquet")))
  }
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Start / Stop lifecycle
# -------------------------------------------------------------------------

#' Start a database session
#'
#' Loads the Parquet-backed store under `db/` into an in-memory SQLite
#' connection and registers lazy `dplyr` table handles and cached tibbles in
#' `gt.env`.
#'
#' @details
#' Requires [initialize_db()] to have been run in the current working
#' directory. All Parquet files are read into an in-memory SQLite instance;
#' the following bindings are written to `gt.env`:
#' \describe{
#'   \item{`globaltrends_db`}{Active `DBI` connection to the in-memory SQLite
#'     instance.}
#'   \item{`keywords_control`, `keywords_object`}{Data frames of control and
#'     object keywords by batch (without the `type` column).}
#'   \item{`time_control`, `time_object`}{Data frames of batch time windows for
#'     control and object runs (without the `type` column).}
#'   \item{`keyword_synonyms`}{Data frame of all keyword/synonym pairs.}
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
#' @importFrom dbplyr sql
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl

start_db <- function() {
  check <- .check_files()
  if (!isTRUE(all(check))) {
    stop(
      "Database files do not exist under 'db/'. Run `initialize_db()` first.",
      call. = FALSE
    )
  }

  con <- dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  assign("globaltrends_db", con, envir = gt.env)

  # Import Parquet tables into the in-memory DB (ReadableFile avoids
  # memory-mapping so the files can be overwritten later on Windows)
  tables <- .list_files()
  for (tbl_name in tables) {
    pq_path <- file.path("db", paste0(tbl_name, ".parquet"))
    raw_file <- arrow::ReadableFile$create(pq_path)
    df <- as.data.frame(arrow::read_parquet(raw_file))
    raw_file$close()
    DBI::dbWriteTable(con, tbl_name, df)
  }

  # Cache small, frequently-used tables as in-memory data frames
  keywords_control <- DBI::dbGetQuery(con, "SELECT batch, keyword FROM batch_keywords WHERE type = 'control'")
  keywords_object <- DBI::dbGetQuery(con, "SELECT batch, keyword FROM batch_keywords WHERE type = 'object'")
  time_control <- DBI::dbGetQuery(con, "SELECT batch, start_date, end_date FROM batch_time WHERE type = 'control'")
  time_object <- DBI::dbGetQuery(con, "SELECT batch, start_date, end_date FROM batch_time WHERE type = 'object'")
  keyword_synonyms <- DBI::dbGetQuery(con, "SELECT * FROM keyword_synonyms")

  # Assign into gt.env
  lst_object <- list(
    globaltrends_db  = con,
    keywords_control = keywords_control,
    keywords_object  = keywords_object,
    time_control     = time_control,
    time_object      = time_object,
    keyword_synonyms = keyword_synonyms,
    tbl_locations    = dplyr::tbl(con, "data_locations"),
    tbl_keywords     = dplyr::tbl(con, "batch_keywords"),
    tbl_time         = dplyr::tbl(con, "batch_time"),
    tbl_synonyms     = dplyr::tbl(con, "keyword_synonyms"),
    tbl_control      = dplyr::tbl(con, "data_control"),
    tbl_object       = dplyr::tbl(con, "data_object"),
    tbl_score        = dplyr::tbl(con, "data_score"),
    tbl_doi          = dplyr::tbl(con, "data_doi"),
    tbl_related      = dplyr::tbl(con, "data_related"),
    tbl_region       = dplyr::tbl(con, "data_region")
  )
  invisible(list2env(lst_object, envir = gt.env))

  .export_locations()

  message("Successfully loaded database and exported table handles to gt.env.")
  invisible(TRUE)
}

#' Disconnect from the database and persist changes
#'
#' Exports the current in-memory SQLite state to the Parquet store under
#' `db/` and closes the DBI connection.
#'
#' @details
#' Call this function after all downloads and computations are complete. It
#' overwrites the Parquet files under `db/` with the current in-memory state
#' and then closes the SQLite connection. `gt.env$globaltrends_db` is set
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

  dbDisconnect(conn = gt.env$globaltrends_db)

  nulls <- list(
    globaltrends_db = NULL,
    tbl_locations   = NULL,
    tbl_keywords    = NULL,
    tbl_time        = NULL,
    tbl_synonyms    = NULL,
    tbl_control     = NULL,
    tbl_object      = NULL,
    tbl_score       = NULL,
    tbl_doi         = NULL,
    tbl_related     = NULL,
    tbl_region      = NULL
  )
  invisible(list2env(nulls, envir = gt.env))

  message("Successfully disconnected and persisted database to 'db/'.")
  invisible(TRUE)
}
