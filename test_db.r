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
new_control <- add_control_keyword(keyword = c("gmail", "wikipedia"), time = "2016-01-01 2019-12-31")
filter(batch_terms, batch == new_control & type == "con")

# add new object batch ----
new_object <- add_object_keyword(keyword = c("manchester united", "real madrid"), time = "2016-01-01 2019-12-31")
filter(batch_terms, batch == new_object & type == "obj")

# run control download ----
download_control(control = new_control, locations = lst_wdi[1:5])
download_control(control = new_control, locations = lst_usa[1:5])
filter(data_con, batch == new_control)

# run object download ----
download_object(object = new_object, locations = lst_wdi[1:5])
download_object(object = new_object, locations = lst_usa[1:5])
filter(data_obj, batch == new_object)

# run map download ----
download_mapping(control = new_control, object = new_object, locations = lst_wdi)
download_mapping(control = new_control, object = new_object, locations = lst_usa)
filter(data_map, batch_c == new_control & batch_o == new_object)

# run scoring ----
compute_score(control = new_control, object = new_object, locations = lst_wdi)
compute_score(control = new_control, object = new_object, locations = lst_usa)
filter(data_score, batch_c == new_control & batch_o == new_object)

# run aggregation ----
compute_doi(control = new_control, object = new_object, locations = "lst_wdi")
compute_doi(control = new_control, object = new_object, locations = "lst_usa")
filter(data_agg, batch_c == new_control & batch_o == new_object)

# run world download ----
download_global(object = new_object)
filter(data_wrld, batch == new_object)

# remove data ----
remove_data(table = "batch_terms", control = new_control)
remove_data(table = "batch_terms", object = new_object)

filter(batch_terms, batch == new_control & type == "con")
filter(batch_terms, batch == new_object & type == "obj")
filter(data_con, batch == new_control)
filter(data_obj, batch == new_object)
filter(data_map, batch_c == new_control & batch_o == new_object)
filter(data_score, batch_c == new_control & batch_o == new_object)
filter(data_wrld, batch == new_object)

# disconnect from db ----
disconnect_db()
