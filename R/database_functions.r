#' @title Initialize database
#'
#' @description
#' The function creates a new database for the `globaltrends` package and
#' creates all necessary tables within the database.
#'
#' @details
#' The function creates a new SQLite database for the `globaltrends`
#' package. The database is saved as file *db/globaltrends_db.sqlite* in
#' the working directory. If the folder *db* does not exists in the working
#' directory, the folder is created. If the database already exists in the
#' working directory, the function exits with an error. Within the database all
#' tables are created and the default location sets are added to the respective
#' table:
#' \itemize{
#'   \item *countries* - all countries with a share in global GDP >= 0.1\%
#'   in 2018.
#'   \item *us_states* - all US federal states and Washington DC.
#' }
#' After creating the database, the function disconnects from the database.
#'
#' @section Warning:
#' SQLite databases only allow one writer at any instant in time. To run
#' parallel downloads use one database for each download client and merge them
#' once all downloads are complete.
#'
#' @seealso
#' * [start_db()]
#' * [disconnect_db()]
#' * [countries()]
#' * [us_states()]
#' * [example_keywords()]
#' * [example_time()]
#' * [example_control()]
#' * [example_object()]
#' * [example_score()]
#' * [example_doi()]
#' * <https://www.sqlite.org/index.html>
#'
#' @return Database is created.
#'
#' @examples
#' \dontrun{
#' initialize_db()
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom purrr walk

initialize_db <- function() {
  if (!dir.exists("db")) {
    dir.create("db")
  }

  check <- .check_files()
  if (all(check)) {
    message("Database files do already exist.")
    return(NULL)
  } else {
    lst_schema <- c(
      "CREATE TABLE batch_keywords(type VARCHAR, batch INTEGER, keyword VARCHAR);",
      "CREATE TABLE batch_time(type VARCHAR, batch INTEGER, start_date VARCHAR, end_date VARCHAR);",
      "CREATE TABLE data_control(location VARCHAR, keyword VARCHAR, date INTEGER, hits FLOAT, batch INTEGER);",
      "CREATE TABLE data_doi(keyword VARCHAR, date INTEGER, type VARCHAR, gini FLOAT, hhi FLOAT, entropy FLOAT, batch_c INTEGER, batch_o INTEGER, locations VARCHAR);",
      "CREATE TABLE data_locations(location VARCHAR, type VARCHAR);",
      "CREATE TABLE data_object(location VARCHAR, keyword VARCHAR, date INTEGER, hits FLOAT, batch_c INTEGER, batch_o INTEGER);",
      "CREATE TABLE data_score(location VARCHAR, keyword VARCHAR, date INTEGER, score FLOAT, batch_c INTEGER, batch_o INTEGER, synonym INTEGER);",
      "CREATE TABLE keyword_synonyms(keyword VARCHAR, synonym VARCHAR);",
      "CREATE INDEX idx_agg ON data_doi(batch_c, batch_o);",
      "CREATE INDEX idx_con ON data_control(batch);",
      "CREATE INDEX idx_location ON data_locations(location);",
      "CREATE INDEX idx_obj ON data_object(batch_c, batch_o);",
      "CREATE INDEX idx_score ON data_score(batch_c, batch_o);",
      "CREATE INDEX idx_terms ON batch_keywords(batch);",
      "CREATE INDEX idx_time ON batch_time(batch);"
    )

    # create in-memory database
    globaltrends_db <- dbConnect(duckdb(), dbdir = ":memory:")
    assign("globaltrends_db", globaltrends_db, envir = gt.env)

    # create tables
    walk(
      lst_schema,
      ~ dbExecute(globaltrends_db, .x)
    )

    # add locations
    .enter_location()

    # write database to parquet
    dbExecute(globaltrends_db, "EXPORT DATABASE 'db' (FORMAT parquet);")
    file.remove(file.path("db", c("load.sql", "schema.sql")))

    # disconnect from db
    dbDisconnect(globaltrends_db)

    message("Database files created successfully.")
  }
}

#' @title List of database files
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd

.list_files <- function() {
  out <- c(
    "batch_keywords",
    "batch_time",
    "data_control",
    "data_doi",
    "data_locations",
    "data_object",
    "data_score",
    "keyword_synonyms"
  )
  return(out)
}

#' @title Check database files
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd

.check_files <- function() {
  lst_files <- paste0(.list_files(), ".parquet")

  check <- file.exists(file.path("db", lst_files))

  if (any(check) & !all(check)) {
    stop(
      paste(
        "Database files appear incomplete. The following files are missing:",
        paste(lst_files[!check], collapse = ", "),
        "."
      )
    )
  } else {
    return(check)
  }
}

#' @title Enter location data into database
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd

.enter_location <- function() {
  # create countries
  add_locations(
    locations = globaltrends::countries,
    type = "countries",
    export = FALSE
  )

  # create us_states
  add_locations(
    locations = globaltrends::us_states,
    type = "us_states",
    export = FALSE
  )
}

#' @title Load globaltrends database and tables
#'
#' @description
#' The function connects to the database file *db/globaltrends_db.sqlite*
#' in the working directory. After connecting to the database connections to the
#' database tables (through `dplyr::tbl`) are created. Data from the tables
#' *batch_keywords* and *batch_time* are exported to the `tibble`
#' objects *keywords_control*, *keywords_object*, *time_control*,
#' and *time_object*.
#'
#' @section Warning:
#' SQLite databases only allow one writer at any instant in time. To run
#' parallel downloads use one database for each download client and merge them
#' once all downloads are complete.
#'
#' @seealso
#' * [initialize_db()]
#' * [disconnect_db()]
#' * [dplyr::tbl()]
#'
#' @return
#' The function exports the following objects to the package environment `globaltrends_db`:
#' \itemize{
#'   \item globaltrends_db A DBIConnection object, as returned by
#'   `DBI::dbConnect()`, connecting to the SQLite database in the working
#'   directory
#'   \item tbl_doi A remote data source pointing to the table *data_doi* in
#'   the connected SQLite database
#'   \item tbl_control A remote data source pointing to the table
#'   *data_control* in the connected SQLite database
#'   \item tbl_object A remote data source pointing to the table
#'   *data_object* in the connected SQLite database
#'   \item tbl_score A remote data source pointing to the table
#'   *data_score* in the connected SQLite database
#' \item countries A `character` vector containing ISO2 country codes of
#'   countries that add at least 0.1\% to global GDP
#'   \item us_states A `character` vector containing ISO2 regional codes of
#'   US states
#'   \item keywords_control A `tibble` containing keywords of control
#'   batches
#'   \item time_control A `tibble` containing times of control batches
#'   \item keywords_object A `tibble` containing keywords of object batches
#'   \item time_object A `tibble` containing times of control batches
#'   \item keyword_synonyms A `tibble` containing synonymous keywords
#' }
#'
#' @examples
#' \dontrun{
#' start_db()
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom duckdb duckdb
#' @importFrom dplyr collect
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr tbl
#' @importFrom rlang .data

start_db <- function() {
  # connect to db --------------------------------------------------------------
  check <- .check_files()
  if (!all(check)) {
    stop(
      paste(
        "Error: Database files do not exist in working directory.",
        "Create database files first.",
        collapse = "\n"
      )
    )
  }

  # create in-memory database
  globaltrends_db <- dbConnect(duckdb(), dbdir = ":memory:")
  assign("globaltrends_db", globaltrends_db, envir = gt.env)

  # fill tables
  lst_sql <- c(
    paste0(
      "CREATE TABLE ",
      .list_files(),
      " AS SELECT * FROM read_parquet('db/",
      .list_files(),
      ".parquet');"
    )
  )
  walk(
    lst_sql,
    ~ dbExecute(globaltrends_db, .x)
  )

  # get tables
  tbl_locations <- tbl(globaltrends_db, "data_locations")
  tbl_keywords <- tbl(globaltrends_db, "batch_keywords")
  tbl_time <- tbl(globaltrends_db, "batch_time")
  tbl_synonyms <- tbl(globaltrends_db, "keyword_synonyms")

  tbl_doi <- tbl(globaltrends_db, "data_doi")
  tbl_control <- tbl(globaltrends_db, "data_control")
  tbl_object <- tbl(globaltrends_db, "data_object")
  tbl_score <- tbl(globaltrends_db, "data_score")

  # load files -----------------------------------------------------------------
  keywords_control <- filter(tbl_keywords, .data$type == "control")
  keywords_control <- select(keywords_control, -type)
  keywords_control <- collect(keywords_control)
  time_control <- filter(tbl_time, .data$type == "control")
  time_control <- select(time_control, -type)
  time_control <- collect(time_control)
  keywords_object <- filter(tbl_keywords, .data$type == "object")
  keywords_object <- select(keywords_object, -type)
  keywords_object <- collect(keywords_object)
  time_object <- filter(tbl_time, .data$type == "object")
  time_object <- select(time_object, -type)
  time_object <- collect(time_object)
  keyword_synonyms <- collect(tbl_synonyms)

  # write objects to the package environment gt.env ----------------------------
  lst_object <- list(
    globaltrends_db,
    tbl_locations,
    tbl_keywords,
    tbl_time,
    tbl_synonyms,
    tbl_doi,
    tbl_control,
    tbl_object,
    tbl_score,
    keywords_control,
    time_control,
    keywords_object,
    time_object,
    keyword_synonyms
  )
  names(lst_object) <- list(
    "globaltrends_db",
    "tbl_locations",
    "tbl_keywords",
    "tbl_time",
    "tbl_synonyms",
    "tbl_doi",
    "tbl_control",
    "tbl_object",
    "tbl_score",
    "keywords_control",
    "time_control",
    "keywords_object",
    "time_object",
    "keyword_synonyms"
  )
  invisible(list2env(lst_object, envir = gt.env))

  .export_locations()
  message("Successfully exported all objects to package environment gt.env.")
}

#' @title Disconnect from database
#'
#' @description
#' The function closes the connection to the database file
#' *db/globaltrends_db.sqlite* in the working directory.
#'
#' @section Warning:
#' SQLite databases only allow one writer at any instant in time. To run
#' parallel downloads use one database for each download client and merge them
#' once all downloads are complete.
#'
#' @seealso
#' * [initialize_db()]
#' * [start_db()]
#'
#' @return
#' Message that disconnection was successful.
#'
#' @examples
#' \dontrun{
#' disconnect_db()
#' }
#' @export
#' @importFrom DBI dbDisconnect

disconnect_db <- function() {
  dbDisconnect(conn = gt.env$globaltrends_db)
  message("Successfully disconnected.")
}
