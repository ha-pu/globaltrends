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
filter(batch_keywords, batch == new_control & type == "con")

# add new object batch ----
new_object <- add_object_keyword(keyword = c("manchester united", "real madrid"), time = "2016-01-01 2019-12-31")
filter(batch_keywords, batch == new_object & type == "obj")

# run control download ----
download_control(control = new_control, locations = countries[1:5])
download_control(control = new_control, locations = us_states[1:5])
filter(data_control, batch == new_control)

# run object download ----
download_object(object = new_object, locations = countries[1:5])
download_object(object = new_object, locations = us_states[1:5])
filter(data_object, batch == new_object)

# run map download ----
download_mapping(control = new_control, object = new_object, locations = countries)
download_mapping(control = new_control, object = new_object, locations = us_states)
filter(data_mapping, batch_c == new_control & batch_o == new_object)

# run scoring ----
compute_score(control = new_control, object = new_object, locations = countries)
compute_score(control = new_control, object = new_object, locations = us_states)
filter(data_score, batch_c == new_control & batch_o == new_object)

# run aggregation ----
compute_doi(control = new_control, object = new_object, locations = "countries")
compute_doi(control = new_control, object = new_object, locations = "us_states")
filter(data_doi, batch_c == new_control & batch_o == new_object)

# run world download ----
download_global(object = new_object)
filter(data_global, batch == new_object)

# remove data ----
remove_data(table = "batch_keywords", control = new_control)
remove_data(table = "batch_keywords", object = new_object)

filter(batch_keywords, batch == new_control & type == "con")
filter(batch_keywords, batch == new_object & type == "obj")
filter(data_control, batch == new_control)
filter(data_object, batch == new_object)
filter(data_mapping, batch_c == new_control & batch_o == new_object)
filter(data_score, batch_c == new_control & batch_o == new_object)
filter(data_global, batch == new_object)

# disconnect from db ----
disconnect_db()
