# run data base test

# packages ----
library(doiGT)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

# connect to db ----
dir_wd <- tempdir()
setwd(dir_wd)
initialize_db()
start_db()

# add new control batch ----
new_batch_c <- add_control_keyword(keyword = c("gmail", "wikipedia"), time = "2016-01-01 2019-12-31")
filter(batch_terms, batch == new_batch_c & type == "con")

# add new object batch ----
new_batch_o <- add_object_keyword(keyword = c("manchester united", "real madrid"), time = "2016-01-01 2019-12-31")
filter(batch_terms, batch == new_batch_o & type == "obj")

# run control download ----
download_control(control = new_batch_c, locations = lst_wdi[1:5])
download_control(control = new_batch_c, locations = lst_usa[1:5])
filter(data_con, batch == new_batch_c)

# run object download ----
download_object(object = new_batch_o, locations = lst_wdi[1:5])
download_object(object = new_batch_o, locations = lst_usa[1:5])
filter(data_obj, batch == new_batch_o)

# run map download ----
download_mapping(control = new_batch_c, object = new_batch_o, locations = lst_wdi)
download_mapping(control = new_batch_c, object = new_batch_o, locations = lst_usa)
filter(data_map, batch_c == new_batch_c & batch_o == new_batch_o)

# run scoring ----
compute_score(control = new_batch_c, object = new_batch_o, locations = lst_wdi)
compute_score(control = new_batch_c, object = new_batch_o, locations = lst_usa)
filter(data_score, batch_c == new_batch_c & batch_o == new_batch_o)

# run aggregation ----
compute_doi(control = new_batch_c, object = new_batch_o, locations = "lst_wdi")
compute_doi(control = new_batch_c, object = new_batch_o, locations = "lst_usa")
filter(data_agg, batch_c == new_batch_c & batch_o == new_batch_o)

# run world download ----
download_global(object = new_batch_o)
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
disconnect_db()
