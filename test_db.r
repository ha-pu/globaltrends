# run data base test

# packages ----
library(DBI)
library(dbplyr)
library(lubridate)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

# connect to db ----
gtrends_db_file <- "db/gtrends.sqlite"
gtrends_db_file_bu <- "db/gtrends_bu.sqlite"
file.copy(gtrends_db_file, gtrends_db_file_bu)
gtrends_db <- dbConnect(RSQLite::SQLite(), gtrends_db_file)
src_dbi(gtrends_db)

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
time_obj  <- filter(batch_time, type == "obj")
time_obj <- collect(time_obj)
dict_obj <- collect(dict_obj)

# add new control batch ----
add_batch(type = "control", keyword = c("gmail", "wikipedia"), time = "2016-01-01 2019-12-31")
new_batch_c <- max(terms_con$batch)
filter(batch_terms, batch == new_batch_c & type == "con")

# add new object batch ----
add_batch(type = "object", keyword = c("manchester united", "real madrid"), time = "2016-01-01 2019-12-31")
new_batch_o <- max(terms_obj$batch)
filter(batch_terms, batch == new_batch_o & type == "obj")

# run control download ----
run_control(control = new_batch_c, lst_geo = lst_wdi[1:5])
filter(data_con, batch == new_batch_c)

# run object download ----
run_object(object = new_batch_o, lst_geo = lst_wdi[1:5])
filter(data_obj, batch == new_batch_o)

# run map download ----
run_map(control = new_batch_c, object = new_batch_o)
filter(data_map, batch_c == new_batch_c & batch_o == new_batch_o)

# run scoring ----
run_score(control = new_batch_c, object = new_batch_o)
filter(data_score, batch_c == new_batch_c & batch_o == new_batch_o)

# run aggregation ----
run_agg(control = new_batch_c, object = new_batch_o)
filter(data_agg, batch_c == new_batch_c & batch_o == new_batch_o)

# run world download ----
run_wrld(object = new_batch_o)
filter(data_wrld, batch == new_batch_o)

# remove data ----
run_remove(table = "batch_terms", batch_c = new_batch_c)
run_remove(table = "batch_terms", batch_o = new_batch_o)

filter(batch_terms, batch == new_batch_c & type == "con")
filter(batch_terms, batch == new_batch_o & type == "obj")
filter(data_con, batch == new_batch_c)
filter(data_obj, batch == new_batch_o)
filter(data_map, batch_c == new_batch_c & batch_o == new_batch_o)
filter(data_score, batch_c == new_batch_c & batch_o == new_batch_o)
filter(data_wrld, batch == new_batch_o)

# disconnect from db ----
DBI::dbDisconnect(gtrends_db)
file.remove(gtrends_db_file_bu)
