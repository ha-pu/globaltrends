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
#' run_init()
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbExecute
#' @importFrom dplyr src_sqlite
#' @importFrom RSQLite SQLite

run_init <- function() {

  # create db folder ----
  if (!dir.exists("db")) dir.create("db")

  # create db ----
  gtrends_db_file <- file.path("db/gtrends.sqlite")
  gtrends_db <- suppressWarnings(src_sqlite(gtrends_db_file, create = TRUE))
  gtrends_db <- dbConnect(SQLite(), gtrends_db_file)
  message("Database has been created.")

  # create tables ----

  # batch_terms
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE batch_terms (
  type TEXT,
  batch INTEGER,
  keyword TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_terms ON batch_terms (batch);")
  message("Table 'batch_terms' has been created.")

  # batch_time
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE batch_time (
  type TEXT,
  batch INTEGER,
  time TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_time ON batch_time (batch);")
  message("Table 'batch_time' has been created.")
  
  # dict_obj
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE dict_obj (
  term1 TEXT,
  term2 TEXT
          )")
  message("Table 'dict_obj' has been created.")
  
  # data_geo
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_geo (
  name TEXT,
  geo TEXT,
  share REAL,
  cum_share REAL,
  type TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_geo ON data_geo (geo);")
  message("Table 'data_geo' has been created.")
  .enter_geo(gtrends_db = gtrends_db)

  # data_con
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_con (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_con ON data_con (batch);")
  message("Table 'batch_terms' has been created.")
  
  # data_obj
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_obj (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_obj ON data_obj (batch);")
  message("Table 'data_con' has been created.")
  
  # data_map
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_map (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_map ON data_map (batch_c, batch_o);")
  message("Table 'data_map' has been created.")
  
  # data_score
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_score (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  score_obs REAL,
  score_sad REAL,
  score_trd REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_score ON data_score (batch_c, batch_o);")
  message("Table 'data_score' has been created.")
  
  # data_agg
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_agg (
  keyword TEXT,
  date INTEGER,
  type TEXT,
  gini REAL,
  hhi REAL,
  entropy REAL,
  batch_c INTEGER,
  batch_o INTEGER,
  lst_geo TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_agg ON data_agg (batch_c, batch_o);")
  message("Table 'data_agg' has been created.")
  
  # data_wrld
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_wrld (
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_wrld ON data_wrld (batch);")
  message("Table 'data_wrld' has been created.")
  
  # disconnect from db ----
  disconnect_db()
}
