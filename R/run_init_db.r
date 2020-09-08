#' @title
#' @description
#' @details
#' @seealso
#' @return
#' @example
#' @export
#' @importFrom

run_init <- function() {

  # create db folder ----
  if (!dir.exists("db")) dir.create("db")

  # create db ----
  gtrends_db_file <- file.path("db/gtrends.sqlite")
  gtrends_db <- dplyr::src_sqlite(gtrends_db_file, create = TRUE)
  gtrends_db <- DBI::dbConnect(RSQLite::SQLite(), gtrends_db_file)

  # create tables ----

  # batch_terms
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE batch_terms (
  type TEXT,
  batch INTEGER,
  keyword TEXT
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_terms ON batch_terms (batch);")


  # batch_time
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE batch_time (
  type TEXT,
  batch INTEGER,
  time TEXT
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_time ON batch_time (batch);")

  # dict_obj
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE dict_obj (
  term1 TEXT,
  term2 TEXT
          )")

  # data_geo
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_geo (
  name TEXT,
  geo TEXT,
  share REAL,
  cum_share REAL,
  type TEXT
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_geo ON data_geo (geo);")
  .enter_geo(gtrends_db = gtrends_db)

  # data_con
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_con (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_con ON data_con (batch);")

  # data_obj
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_obj (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_obj ON data_obj (batch);")

  # data_map
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_map (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_map ON data_map (batch_c, batch_o);")

  # data_score
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_score (
  geo TEXT,
  keyword TEXT,
  date INTEGER,
  score_obs REAL,
  score_sad REAL,
  score_trd REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_score ON data_score (batch_c, batch_o);")

  # data_agg
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_agg (
  keyword TEXT,
  date INTEGER,
  type TEXT,
  gini REAL,
  hhi REAL,
  entropy REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_agg ON data_agg (batch_c, batch_o);")

  # data_wrld
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_wrld (
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  DBI::dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_wrld ON data_wrld (batch);")

  # show database
  dbplyr::src_dbi(gtrends_db)

  # disconnect from db ----
  DBI::dbDisconnect(gtrends_db)
}
