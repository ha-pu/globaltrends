# packages ----
library(DBI)
library(dbplyr)
library(lubridate)
library(tidyverse)
options(dplyr.summarise.inform = FALSE) 

# parameter ----
setwd("..")

# connect to db ----
gtrends_db_file <- "db/gtrends.sqlite"
gtrends_db <- dbConnect(RSQLite::SQLite(), gtrends_db_file)
src_dbi(gtrends_db)

# source files ----
source("code/hlpr_test_empty.r")
source("code/hlpr_get_trend.r")


source("code/add_batch_db.r")
source("code/run_agg_db.r")
source("code/run_control_db.r")
# source("code/run_doi.r")
source("code/run_map_db.r")
source("code/run_object_db.r") 
source("code/run_remove_db.r")
source("code/run_score_db.r")
source("code/run_wrld_db.r")

# get tables ----
data_geo <- tbl(gtrends_db, "data_geo")
batch_terms <- tbl(gtrends_db, "batch_terms")
batch_time <- tbl(gtrends_db, "batch_time")
dict_obj <- tbl(gtrends_db, "dict_obj")

data_agg <- tbl(gtrends_db, "data_agg")
data_con <- tbl(gtrends_db, "data_con")
data_map <- tbl(gtrends_db, "data_map")
data_obj <- tbl(gtrends_db, "data_obj")
data_score <- tbl(gtrends_db, "data_score")
data_wrld <- tbl(gtrends_db, "data_wrld")

# load files ----
lst_wdi <- filter(data_geo, type == "lst_wdi" & share >= 0.001)
lst_wdi <- collect(lst_wdi)
lst_wdi <- pull(lst_wdi, geo)
lst_usa <- filter(data_geo, type == "lst_usa")
lst_usa <- collect(lst_usa)
lst_usa <- pull(lst_usa, geo)

terms_con <- filter(batch_terms, type == "con")
terms_con <- collect(terms_con)
time_con <- filter(batch_time, type == "con")
time_con <- collect(time_con)
terms_obj <- filter(batch_terms, type == "obj")
terms_obj <- collect(terms_obj)
time_obj  <- filter(batch_terms, type == "obj")
time_obj <- collect(time_obj)
dict_obj <- collect(dict_obj)

# disconnect from db ----
# DBI::dbDisconnect(gtrends_db)