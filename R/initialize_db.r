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
  doiGT_DB <- suppressWarnings(src_sqlite("db/doiGT_DB.sqlite", create = TRUE))
  doiGT_DB <- dbConnect(SQLite(), "db/doiGT_DB.sqlite")
  message("Database has been created.")

  # create tables ----

  # batch_keywords
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE batch_keywords (
  type TEXT,
  batch INTEGER,
  keyword TEXT
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_terms ON batch_keywords (batch);")
  message("Table 'batch_keywords' has been created.")

  # batch_time
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE batch_time (
  type TEXT,
  batch INTEGER,
  time TEXT
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_time ON batch_time (batch);")
  message("Table 'batch_time' has been created.")

  # dict_obj
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE dict_obj (
  term1 TEXT,
  term2 TEXT
          )")
  message("Table 'dict_obj' has been created.")

  # data_geo
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_geo (
  name TEXT,
  geo TEXT,
  share REAL,
  cum_share REAL,
  type TEXT
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_geo ON data_geo (geo);")
  message("Table 'data_geo' has been created.")
  .enter_geo(doiGT_DB = doiGT_DB)

  # data_control
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_control (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_con ON data_control (batch);")
  message("Table 'batch_keywords' has been created.")

  # data_object
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_object (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_obj ON data_object (batch);")
  message("Table 'data_control' has been created.")

  # data_map
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_map (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_map ON data_map (batch_c, batch_o);")
  message("Table 'data_map' has been created.")

  # data_score
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_score (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  score_obs REAL,
  score_sad REAL,
  score_trd REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_score ON data_score (batch_c, batch_o);")
  message("Table 'data_score' has been created.")

  # data_agg
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_agg (
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
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_agg ON data_agg (batch_c, batch_o);")
  message("Table 'data_agg' has been created.")

  # data_global
  dbExecute(conn = doiGT_DB, statement = "CREATE TABLE data_global (
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = doiGT_DB, statement = "CREATE INDEX idx_wrld ON data_global (batch);")
  message("Table 'data_global' has been created.")

  # disconnect from db ----
  disconnect_db(db = doiGT_DB)
}
