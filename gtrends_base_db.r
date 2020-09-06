# parameter ----
setwd("..")
options(dplyr.summarise.inform = FALSE) 

# connect to db ----
gtrends_db_file <- "db/gtrends.sqlite"
gtrends_db <- DBI::dbConnect(RSQLite::SQLite(), gtrends_db_file)
dplyr::src_dbi(gtrends_db)

# get tables ----
data_geo <- dplyr::tbl(gtrends_db, "data_geo")
batch_terms <- dplyr::tbl(gtrends_db, "batch_terms")
batch_time <- dplyr::tbl(gtrends_db, "batch_time")
dict_obj <- dplyr::tbl(gtrends_db, "dict_obj")

data_agg <- dplyr::tbl(gtrends_db, "data_agg")
data_con <- dplyr::tbl(gtrends_db, "data_con")
data_map <- dplyr::tbl(gtrends_db, "data_map")
data_obj <- dplyr::tbl(gtrends_db, "data_obj")
data_score <- dplyr::tbl(gtrends_db, "data_score")
data_wrld <- dplyr::tbl(gtrends_db, "data_wrld")

# load files ----
lst_wdi <- dplyr::filter(data_geo, type == "lst_wdi" & share >= 0.001)
lst_wdi <- dplyr::collect(lst_wdi)
lst_wdi <- dplyr::pull(lst_wdi, geo)
lst_usa <- dplyr::filter(data_geo, type == "lst_usa")
lst_usa <- dplyr::collect(lst_usa)
lst_usa <- dplyr::pull(lst_usa, geo)

terms_con <- dplyr::filter(batch_terms, type == "con")
terms_con <- dplyr::collect(terms_con)
time_con <- dplyr::filter(batch_time, type == "con")
time_con <- dplyr::collect(time_con)
terms_obj <- dplyr::filter(batch_terms, type == "obj")
terms_obj <- dplyr::collect(terms_obj)
time_obj  <- dplyr::filter(batch_terms, type == "obj")
time_obj <- dplyr::collect(time_obj)
dict_obj <- dplyr::collect(dict_obj)

# disconnect from db ----
# DBI::dbDisconnect(gtrends_db)
