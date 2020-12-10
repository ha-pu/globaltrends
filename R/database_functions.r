#' @title Initialize database
#'
#' @description
#' The function creates a new database for the \code{globaltrends} package and
#' creates all necessary tables within the database.
#'
#' @details
#' The function creates a new SQLite database for the \code{globaltrends}
#' package. The database is saved as file \emph{db/globaltrends_db.sqlite} in
#' the working directory. If the folder \emph{db} does not exists in the working
#' directory, the folder is created. If the database already exists in the
#' working directory, the function exits with an error. Within the database all
#' tables are created and the default location sets are added to the respective
#' table:
#' \itemize{
#'   \item \emph{countries} - all countries with a share in global GDP >= 0.1\%
#'   in 2018.
#'   \item \emph{us_states} - all US federal states and Washington DC.
#' }
#' After creating the database, the function disconnects from the database.
#'
#' @seealso
#' * \code{\link{start_db}}
#' * \code{\link{disconnect_db}}
#' * \code{\link{batch_keywords}}
#' * \code{\link{batch_time}}
#' * \code{\link{data_control}}
#' * \code{\link{data_object}}
#' * \code{\link{data_score}}
#' * \code{\link{data_doi}}
#' * \url{https://www.sqlite.org/index.html}
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
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom RSQLite SQLite

initialize_db <- function() {

  # create db folder -----------------------------------------------------------
  if (!dir.exists("db")) dir.create("db")

  # create db ------------------------------------------------------------------
  if (file.exists("db/globaltrends_db.sqlite")) stop("Error: File 'db/globaltrends_db.sqlite' already exists.")
  globaltrends_db <- dbConnect(SQLite(), "db/globaltrends_db.sqlite")
  message("Successfully created database.")

  # batch_keywords -------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE batch_keywords (
  type TEXT,
  batch INTEGER,
  keyword TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_terms ON batch_keywords (batch);")
  message("Successfully created table 'batch_keywords'.")

  # batch_time -----------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE batch_time (
  type TEXT,
  batch INTEGER,
  time TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_time ON batch_time (batch);")
  message("Successfully created table 'batch_time'.")

  # keyword_synonyms -----------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE keyword_synonyms (
  keyword TEXT,
  synonym TEXT
          )")
  message("Successfully created table 'keyword_synonyms'.")

  # data_locations -------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_locations (
  name TEXT,
  location TEXT,
  type TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_location ON data_locations (location);")
  message("Successfully created table 'data_locations'.")
  .enter_location(globaltrends_db = globaltrends_db)

  # data_control ---------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_control (
  location TEXT,
  keyword TEXT,
  date INTEGER,
  hits REAL,
  batch INTEGER
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_con ON data_control (batch);")
  message("Successfully created table 'batch_keywords'.")

  # data_object ----------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_object (
  location TEXT,
  keyword TEXT,
  date INTEGER,
  hits REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_obj ON data_object (batch_c, batch_o);")
  message("Successfully created table 'data_control'.")

  # data_score -----------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_score (
  location TEXT,
  keyword TEXT,
  date INTEGER,
  score_obs REAL,
  score_sad REAL,
  score_trd REAL,
  batch_c INTEGER,
  batch_o INTEGER,
  synonym INTEGER
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_score ON data_score (batch_c, batch_o);")
  message("Successfully created table 'data_score'.")

  # data_doi -------------------------------------------------------------------
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_doi (
  keyword TEXT,
  date INTEGER,
  type TEXT,
  gini REAL,
  hhi REAL,
  entropy REAL,
  batch_c INTEGER,
  batch_o INTEGER,
  locations TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_agg ON data_doi (batch_c, batch_o);")
  message("Successfully created table 'data_doi'.")

  # disconnect from db ---------------------------------------------------------
  disconnect_db(db = globaltrends_db)
}

#' @title Enter location data into database
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @importFrom DBI dbWriteTable
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
  countries <- WDI::WDI_data$country
  countries <- as_tibble(countries)
  countries <- filter(countries, .data$region != "Aggregates")
  countries <- select(countries, location = .data$iso2c)
  countries <- WDI::WDI(country = countries$location, indicator = "NY.GDP.MKTP.KD", start = 2018, end = 2018)
  countries <- bind_rows(countries, tibble(iso2c = "TW", country = "Taiwan", NY.GDP.MKTP.KD = 6.08186e+11, year = 2018))
  countries <- mutate(countries, NY.GDP.MKTP.KD = case_when(is.na(.data$NY.GDP.MKTP.KD) ~ 0, TRUE ~ .data$NY.GDP.MKTP.KD))
  countries <- mutate(countries, gdp_share = .data$NY.GDP.MKTP.KD / sum(.data$NY.GDP.MKTP.KD))
  countries <- arrange(countries, -.data$gdp_share)
  countries <- mutate(countries, gdp_cum_share = cumsum(.data$gdp_share))
  countries <- filter(countries, .data$iso2c %in% unique(gtrendsR::countries$country_code) & .data$gdp_share >= 0.001)
  countries <- select(countries, location = .data$iso2c, name = .data$country)
  countries <- mutate(countries, type = "countries")

  # create us_states -----------------------------------------------------------
  us_states <- gtrendsR::countries
  us_states <- mutate_all(us_states, as.character)
  us_states <- us_states[which(us_states$sub_code == "US-AL")[[1]]:which(us_states$sub_code == "US-DC")[[1]], ]
  us_states <- select(us_states, location = .data$sub_code, .data$name)
  us_states <- mutate(us_states, type = "us_states")

  # upload data ----------------------------------------------------------------
  dbWriteTable(conn = globaltrends_db, name = "data_locations", value = bind_rows(countries, us_states), append = TRUE)
  message("Successfully entered data into 'data_locations'.")
}

#' @title Load globaltrends database and tables
#'
#' @description
#' The function connects to the database file \emph{db/globaltrends_db.sqlite}
#' in the working directory. After connecting to the database connections to the
#' database tables (through \code{dplyr::tbl}) are created. Data from the tables
#' \emph{batch_keywords} and \emph{batch_time} are exported to the \code{tibble}
#' objects \emph{keywords_control}, \emph{keywords_object}, \emph{time_control},
#' and \emph{time_object}.
#'
#' @seealso
#' * \code{\link{initialize_db}}
#' * \code{\link{disconnect_db}}
#' * \code{\link[dplyr]{tbl}}
#'
#' @return
#' The function exports the following objects to .GlobalEnv:
#' \itemize{
#'   \item globaltrends_db A DBIConnection object, as returned by
#'   \code{DBI::dbConnect()}, connecting to the SQLite database in the working
#'   directory
#'   \item tbl_doi A remote data source pointing to the table \emph{data_doi} in
#'   the connected SQLite database
#'   \item tbl_control A remote data source pointing to the table
#'   \emph{data_control} in the connected SQLite database
#'   \item tbl_mapping A remote data source pointing to the table
#'   \emph{data_mapping} in the connected SQLite database
#'   \item tbl_object A remote data source pointing to the table
#'   \emph{data_object} in the connected SQLite database
#'   \item tbl_score A remote data source pointing to the table
#'   \emph{data_score} in the connected SQLite database
#'   \item countries A \code{character} vector containing ISO2 country codes of
#'   countries that add at least 0.1\% to global GDP
#'   \item us_states A \code{character} vector containing ISO2 regional codes of
#'   US states
#'   \item keywords_control A \code{tibble} containing keywords of control
#'   batches
#'   \item time_control A \code{tibble} containing times of control batches
#'   \item keywords_object A \code{tibble} containing keywords of object batches
#'   \item time_object A \code{tibble} containing times of control batches
#'   \item keyword_synonyms A \code{tibble} containing synonymous keywords
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
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr tbl
#' @importFrom rlang .data
#' @importFrom RSQLite SQLite

start_db <- function() {
  # connect to db --------------------------------------------------------------
  globaltrends_db <- dbConnect(SQLite(), "db/globaltrends_db.sqlite")
  message("Successfully connected to database.")

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
  locations <- distinct(tbl_locations, type)
  locations <- collect(locations)
  locations <- locations$type
  
  lst_locations <- map(locations, ~ {
    out <- filter(tbl_locations, .data$type == .x)
    out <- collect(out)
    out <- pull(out, .data$location)
    return(out)
  })
  
  names(lst_locations) <- locations

  keywords_control <- filter(tbl_keywords, .data$type == "control")
  keywords_control <- select(keywords_control, -.data$type)
  keywords_control <- collect(keywords_control)
  time_control <- filter(tbl_time, .data$type == "control")
  time_control <- select(time_control, -.data$type)
  time_control <- collect(time_control)
  keywords_object <- filter(tbl_keywords, .data$type == "object")
  keywords_object <- select(keywords_object, -.data$type)
  keywords_object <- collect(keywords_object)
  time_object <- filter(tbl_time, .data$type == "object")
  time_object <- select(time_object, -.data$type)
  time_object <- collect(time_object)
  keyword_synonyms <- collect(tbl_synonyms)

  # write objects to .GlobalEnv ------------------------------------------------
  invisible(list2env(lst_locations, envir = .GlobalEnv))
  
  lst_object <- list(
    tbl_locations,
    tbl_keywords,
    tbl_time,
    tbl_doi,
    tbl_control,
    tbl_object,
    tbl_score,
    tbl_synonyms,
    keywords_control,
    time_control,
    keywords_object,
    time_object,
    keyword_synonyms
  )
  names(lst_object) <- list(
    ".tbl_locations",
    ".tbl_keywords",
    ".tbl_time",
    ".tbl_doi",
    ".tbl_control",
    ".tbl_object",
    ".tbl_score",
    ".tbl_synonyms",
    ".keywords_control",
    ".time_control",
    ".keywords_object",
    ".time_object",
    ".keyword_synonyms"
  )
  invisible(list2env(lst_object, envir = .GlobalEnv))
  lst_object <- list(
    globaltrends_db,
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
  invisible(list2env(lst_object, envir = .GlobalEnv))
  message("Successfully exported all objects to .GlobalEnv.")
}

#' @title Disconnect from database
#'
#' @description
#' The function closes the connection to the database file
#' \emph{db/globaltrends_db.sqlite} in the working directory.
#'
#' @seealso
#' * \code{\link{initialize_db}}
#' * \code{\link{start_db}}
#'
#' @param db Connection to database file that should be closed. Defaults
#' to \code{globaltrends_db}.
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


disconnect_db <- function(db = globaltrends_db) {
  dbDisconnect(conn = db)
  message("Successfully disconnected.")
}
