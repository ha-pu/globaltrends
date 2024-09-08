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
#' @importFrom DBI dbCreateTable
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom dbplyr sql
#' @importFrom RSQLite SQLite

initialize_db <- function() {
  # create db folder -----------------------------------------------------------
  if (!dir.exists("db")) dir.create("db")

  # create db ------------------------------------------------------------------
  if (file.exists("db/globaltrends_db.sqlite")) stop("Error: File 'db/globaltrends_db.sqlite' already exists.")
  globaltrends_db <- dbConnect(SQLite(), "db/globaltrends_db.sqlite")
  assign("globaltrends_db", globaltrends_db, envir = gt.env)
  message("Successfully created database.")

  # batch_keywords -------------------------------------------------------------
  db_cols <- c("TEXT", "INTEGER", "TEXT")
  names(db_cols) <- c("type", "batch", "keyword")
  dbCreateTable(conn = globaltrends_db, name = "batch_keywords", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_terms ON batch_keywords (batch);")
  message("Successfully created table 'batch_keywords'.")

  # batch_time -----------------------------------------------------------------
  db_cols <- c("TEXT", "INTEGER", "TEXT", "TEXT")
  names(db_cols) <- c("type", "batch", "start_date", "end_date")
  dbCreateTable(conn = globaltrends_db, name = "batch_time", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_time ON batch_time (batch);")
  message("Successfully created table 'batch_time'.")

  # keyword_synonyms -----------------------------------------------------------
  db_cols <- c("TEXT", "TEXT")
  names(db_cols) <- c("keyword", "synonym")
  dbCreateTable(conn = globaltrends_db, name = "keyword_synonyms", fields = db_cols)
  message("Successfully created table 'keyword_synonyms'.")

  # data_locations -------------------------------------------------------------
  db_cols <- c("TEXT", "TEXT")
  names(db_cols) <- c("location", "type")
  dbCreateTable(conn = globaltrends_db, name = "data_locations", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_location ON data_locations (location);")
  message("Successfully created table 'data_locations'.")
  .enter_location(globaltrends_db = globaltrends_db)

  # data_control ---------------------------------------------------------------
  db_cols <- c("TEXT", "TEXT", "INTEGER", "REAL", "INTEGER")
  names(db_cols) <- c("location", "keyword", "date", "hits", "batch")
  dbCreateTable(conn = globaltrends_db, name = "data_control", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_con ON data_control (batch);")
  message("Successfully created table 'batch_keywords'.")

  # data_object ----------------------------------------------------------------
  db_cols <- c("TEXT", "TEXT", "INTEGER", "REAL", "INTEGER", "INTEGER")
  names(db_cols) <- c("location", "keyword", "date", "hits", "batch_c", "batch_o")
  dbCreateTable(conn = globaltrends_db, name = "data_object", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_obj ON data_object (batch_c, batch_o);")
  message("Successfully created table 'data_control'.")

  # data_score -----------------------------------------------------------------
  db_cols <- c("TEXT", "TEXT", "INTEGER", "REAL", "INTEGER", "INTEGER", "INTEGER")
  names(db_cols) <- c("location", "keyword", "date", "score", "batch_c", "batch_o", "synonym")
  dbCreateTable(conn = globaltrends_db, name = "data_score", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_score ON data_score (batch_c, batch_o);")
  message("Successfully created table 'data_score'.")

  # data_doi -------------------------------------------------------------------
  db_cols <- c("TEXT", "INTEGER", "REAL", "REAL", "REAL", "INTEGER", "INTEGER", "TEXT")
  names(db_cols) <- c("keyword", "date", "gini", "hhi", "entropy", "batch_c", "batch_o", "locations")
  dbCreateTable(conn = globaltrends_db, name = "data_doi", fields = db_cols)
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_agg ON data_doi (batch_c, batch_o);")
  message("Successfully created table 'data_doi'.")

  # disconnect from db ---------------------------------------------------------
  disconnect_db()
}

#' @title Enter location data into database
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble

.enter_location <- function(globaltrends_db) {
  # create countries -----------------------------------------------------------
  add_locations(
    locations = globaltrends::countries,
    type = "countries",
    export = FALSE
  )

  # create us_states -----------------------------------------------------------
  add_locations(
    locations = globaltrends::us_states,
    type = "us_states",
    export = FALSE
  )

  message("Successfully entered data into 'data_locations'.")
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
#'   \item tbl_mapping A remote data source pointing to the table
#'   *data_mapping* in the connected SQLite database
#'   \item tbl_object A remote data source pointing to the table
#'   *data_object* in the connected SQLite database
#'   \item tbl_score A remote data source pointing to the table
#'   *data_score* in the connected SQLite database
#'   \item countries A `character` vector containing ISO2 country codes of
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
#' @importFrom dplyr collect
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr tbl
#' @importFrom rlang .data
#' @importFrom RSQLite SQLite

start_db <- function() {
  # connect to db --------------------------------------------------------------
  if (file.exists("db/globaltrends_db.sqlite")) {
    globaltrends_db <- dbConnect(SQLite(), "db/globaltrends_db.sqlite")
    message("Successfully connected to database.")
  } else {
    stop("Error: File 'db/globaltrends_db.sqlite' does not exist in working directory.\nSet working directory to correct path.")
  }

  # get tables -----------------------------------------------------------------
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
