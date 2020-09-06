# initialize database

run_init <- function() {
  # packages ----
  library(DBI)
  library(dbplyr)
  library(tidyverse)
  source("code/hlpr_enter_geo.r")
  
  # create db folder ----
  if (!dir.exists("db")) dir.create("db")
  
  # create db ----
  gtrends_db_file <- file.path("db/gtrends.sqlite")
  gtrends_db <- src_sqlite(gtrends_db_file, create = TRUE)
  gtrends_db <- dbConnect(RSQLite::SQLite(), gtrends_db_file)
  
  # create tables ----
  
  # batch_terms
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE batch_terms (
  type TExT,
  batch INTEGER,
  keyword TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_terms ON batch_terms (batch);")
  
  
  # batch_time
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE batch_time (
  type TExT,
  batch INTEGER,
  time TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_time ON batch_time (batch);")
  
  # dict_obj
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE dict_obj (
  term1 TExT,
  term2 TEXT
          )")
  
  # data_geo
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_geo (
  name TExT,
  geo TEXT,
  share REAL,
  cum_share REAL,
  type TEXT
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_geo ON data_geo (geo);")
  .enter_geo()
  
  # data_con
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_con (
  geo TExT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_con ON data_con (batch);")
  
  # data_obj
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_obj (
  geo TExT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_obj ON data_obj (batch);")
  
  # data_map
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_map (
  geo TExT,
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_map ON data_map (batch_c, batch_o);")
  
  # data_score
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_score (
  geo TExT,
  keyword TEXT,
  date INTEGER,
  score_obs REAL,
  score_sad REAL,
  score_trd REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_score ON data_score (batch_c, batch_o);")
  
  # data_agg
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_agg (
  keyword TEXT,
  date INTEGER,
  type TEXT,
  gini REAL,
  hhi REAL,
  entropy REAL,
  batch_c INTEGER,
  batch_o INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_agg ON data_agg (batch_c, batch_o);")
  
  # data_wrld
  dbExecute(conn = gtrends_db, statement = "CREATE TABLE data_wrld (
  keyword TEXT,
  date INTEGER,
  hits INTEGER,
  batch INTEGER
          )")
  dbExecute(conn = gtrends_db, statement = "CREATE INDEX idx_wrld ON data_wrld (batch);")
  
  # show database
  src_dbi(gtrends_db)
  
  # disconnect from db ----
  DBI::dbDisconnect(gtrends_db)
}
