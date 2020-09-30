#' @title Initialize database
#'
#' @description
#' @details
#' @seealso
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
#' @importFrom dplyr src_sqlite
#' @importFrom RSQLite SQLite

initialize_db <- function() {

  # create db folder ----
  if (!dir.exists("db")) dir.create("db")

  # create db ----
  globaltrends_db <- suppressWarnings(src_sqlite("db/globaltrends_db.sqlite", create = TRUE))
  globaltrends_db <- dbConnect(SQLite(), "db/globaltrends_db.sqlite")
  message("Successfully created database.")

  # create tables ----

  # batch_keywords
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE batch_keywords (
  type TEXT,
  batch INTEGER,
  keyword TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_terms ON batch_keywords (batch);")
  message("Successfully created table 'batch_keywords'.")

  # batch_time
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE batch_time (
  type TEXT,
  batch INTEGER,
  time TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_time ON batch_time (batch);")
  message("Successfully created table 'batch_time'.")

  # keyword_synonyms
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE keyword_synonyms (
  keyword TEXT,
  synonym TEXT
          )")
  message("Successfully created table 'keyword_synonyms'.")

  # data_locations
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_locations (
  name TEXT,
  location TEXT,
  type TEXT
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_location ON data_locations (location);")
  message("Successfully created table 'data_locations'.")
  .enter_location(globaltrends_db = globaltrends_db)

  # data_control
  dbExecute(conn = globaltrends_db, statement = "CREATE TABLE data_control (
  location TEXT,
  keyword TEXT,
  date INTEGER,
  hits REAL,
  batch INTEGER
          )")
  dbExecute(conn = globaltrends_db, statement = "CREATE INDEX idx_con ON data_control (batch);")
  message("Successfully created table 'batch_keywords'.")

  # data_object
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

  # data_score
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

  # data_doi
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

  # disconnect from db ----
  disconnect_db(db = globaltrends_db)
}

#' @title Enter location data into database
#'
#' @rdname hlprs
#' @keywords internal
#'
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble

.enter_location <- function(globaltrends_db) {
  # create countries ----
  countries <- WDI::WDI_data$country
  countries <- as_tibble(countries)
  countries <- filter(countries, region != "Aggregates")
  countries <- select(countries, location = iso2c)
  countries <- WDI::WDI(country = countries$location, indicator = "NY.GDP.MKTP.KD", start = 2018, end = 2018)
  countries <- bind_rows(countries, tibble(iso2c = "TW", country = "Taiwan", NY.GDP.MKTP.KD = 6.08186e+11, year = 2018))
  countries <- mutate(countries, NY.GDP.MKTP.KD = case_when(is.na(NY.GDP.MKTP.KD) ~ 0, TRUE ~ NY.GDP.MKTP.KD))
  countries <- mutate(countries, gdp_share = NY.GDP.MKTP.KD / sum(NY.GDP.MKTP.KD))
  countries <- arrange(countries, -gdp_share)
  countries <- mutate(countries, gdp_cum_share = cumsum(gdp_share))
  countries <- filter(countries, iso2c %in% unique(gtrendsR::countries$country_code) & gdp_share >= 0.001)
  countries <- select(countries, location = iso2c, name = country)
  countries <- mutate(countries, type = "countries")

  # create us_states ----
  us_states <- gtrendsR::countries
  us_states <- mutate_all(us_states, as.character)
  us_states <- us_states[which(us_states$sub_code == "US-AL")[[1]]:which(us_states$sub_code == "US-DC")[[1]], ]
  us_states <- select(us_states, location = sub_code, name)
  us_states <- mutate(us_states, type = "us_states")

  # upload data ----
  dbWriteTable(conn = globaltrends_db, name = "data_locations", value = bind_rows(countries, us_states), append = TRUE)
  message("Successfully entered data into 'data_locations'.")
}
