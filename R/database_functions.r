#' @title Initialize the local database store
#'
#' @description
#' Creates the local database store used by `globaltrends` in the current working
#' directory and initializes all required tables and indexes.
#'
#' @details
#' The package uses DuckDB with a Parquet-backed persistence layout under the
#' `db/` folder. `initialize_db()` creates an in-memory DuckDB database,
#' creates the schema, populates default location sets, and exports the database
#' as Parquet files to `db/`.
#'
#' If the database files already exist, the function will not overwrite them.
#' If files exist but are incomplete, the function errors to prevent accidental
#' use of a corrupted store.
#'
#' Default location sets written to `data_locations`:
#' \itemize{
#'   \item `countries`: ISO 3166-1 alpha-2 codes (GDP share threshold; see `countries`).
#'   \item `us_states`: ISO 3166-2 codes for US states and DC (see `us_states`).
#' }
#'
#' @section Concurrency:
#' DuckDB allows concurrent readers but has write constraints depending on the
#' underlying storage and process model. If you run parallel download workers,
#' use one database directory per worker and merge results afterwards.
#'
#' @return Invisibly returns `TRUE` on success. Called for its side effects
#'   (creating files under `db/`).
#'
#' @examples
#' \dontrun{
#' initialize_db()
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
    "CREATE TABLE keyword_synonyms(keyword VARCHAR, synonym VARCHAR);",
    "CREATE INDEX idx_doi_batch ON data_doi(batch_o);",
    "CREATE INDEX idx_control_batch ON data_control(batch);",
    "CREATE INDEX idx_locations_loc ON data_locations(location);",
    "CREATE INDEX idx_regions_term ON data_region(term);",
    "CREATE INDEX idx_regions_loc ON data_region(location);",
    "CREATE INDEX idx_object_batch ON data_object(batch_o);",
    "CREATE INDEX idx_score_batch ON data_score(batch_o);",
    "CREATE INDEX idx_terms_batch ON batch_keywords(batch);",
    "CREATE INDEX idx_time_batch ON batch_time(batch);"
  )

  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  assign("globaltrends_db", con, envir = gt.env)

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

#' @keywords internal
#' @noRd

.ensure_db_dir <- function(path = "db") {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  invisible(TRUE)
}

#' @title List of required tables/files
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
    "keyword_synonyms"
  )
}

#' @title Check database files under `db/`
#' @description
#' Verifies whether all required Parquet files exist. If some but not all are
#' present, the store is treated as corrupted/incomplete and an error is thrown.
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

#' @title Write default location sets into the database
#' @description
#' Inserts the package's default location sets (`countries`, `us_states`) into
#' `data_locations`. This is called during `initialize_db()`.
#' @keywords internal
#' @noRd
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

.enter_location_defaults <- function(con) {
  # Ensure add_locations writes to the connection we just created
  assign("globaltrends_db", con, envir = gt.env)

  data_to_add <- bind_rows(
    tibble(
      location = globaltrends::countries,
      type = "countries"
    ),
    tibble(
      location = globaltrends::us_states,
      type = "us_states"
    )
  )

  dbAppendTable(
    conn = gt.env$globaltrends_db,
    name = "data_locations",
    value = data_to_add
  )

  invisible(TRUE)
}

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

# -------------------------------------------------------------------------
# Start / Stop lifecycle
# -------------------------------------------------------------------------

#' @title Start database (load Parquet store into DuckDB)
#'
#' @description
#' Loads the Parquet-backed database store under `db/` into an in-memory DuckDB
#' connection and registers lazy `dplyr` table handles in `gt.env`.
#'
#' @details
#' `start_db()` requires that [initialize_db()] has been run in the current
#' working directory. It creates an in-memory DuckDB database, reads all Parquet
#' tables from `db/`, and assigns:
#' \itemize{
#'   \item `gt.env$globaltrends_db`: DBI connection (DuckDB).
#'   \item `gt.env$tbl_*`: lazy table references for all tables.
#'   \item `gt.env$keywords_*`, `gt.env$time_*`, `gt.env$keyword_synonyms`: cached tibbles.
#' }
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' start_db()
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
  import_sql <- paste0(
    "CREATE TABLE ",
    .list_files(),
    " AS SELECT * FROM read_parquet('db/",
    .list_files(),
    ".parquet');"
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

  # Cache small, frequently-used tables as in-memory tibbles
  keywords_control <- tbl_keywords |>
    filter(.data$type == "control") |>
    select(-.data$type) |>
    collect()

  keywords_object <- tbl_keywords |>
    filter(.data$type == "object") |>
    select(-.data$type) |>
    collect()

  time_control <- tbl_time |>
    filter(.data$type == "control") |>
    select(-.data$type) |>
    collect()

  time_object <- tbl_time |>
    filter(.data$type == "object") |>
    select(-.data$type) |>
    collect()

  keyword_synonyms <- collect(tbl_synonyms)

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

#' @title Disconnect from the database and persist changes
#'
#' @description
#' Exports the current in-memory DuckDB database to the Parquet store under
#' `db/` and closes the DBI connection.
#'
#' @details
#' Call this after downloads/computations to persist changes to disk. The
#' function overwrites the Parquet files under `db/` with the current in-memory
#' state.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
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
