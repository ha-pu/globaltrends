# setup ----
initialize_db()
start_db()

add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = c(
    "fc barcelona", "fc bayern", "manchester united", "real madrid",
    "amazon", "apple", "facebook", "google",
    "instagram", "microsoft", "netflix", "twitter"
  ),
  time = "2010-01-01 2019-12-31"
)

# download data and compute doi ----
download_control(control = 1, locations = countries[1:2])
download_control_global(control = 1)
download_object(control = 1, object = 1:3, locations = countries[1:2])
download_object_global(control = 1, object = 1)
compute_score(control = 1, object = 1:3, locations = countries[1:2])
compute_score_global(control = 1, object = 1)
compute_doi(control = 1, object = 1:3)

# add_control / add_keyword ----
test_that("add_batch1", {
  expect_error(add_control_keyword(keyword = 1))
  expect_error(add_control_keyword(keyword = TRUE))
  expect_error(add_control_keyword(keyword = sum))
})

test_that("add_batch2", {
  expect_error(add_control_keyword(time = 1))
  expect_error(add_control_keyword(time = TRUE))
  expect_error(add_control_keyword(time = sum))
  expect_error(add_control_keyword(time = letters[1:5]))
})

test_that("add_batch3", {
  expect_error(add_object_keyword(keyword = 1))
  expect_error(add_object_keyword(keyword = TRUE))
  expect_error(add_object_keyword(keyword = sum))
})

test_that("add_batch4", {
  expect_error(add_object_keyword(time = 1))
  expect_error(add_object_keyword(time = TRUE))
  expect_error(add_object_keyword(time = sum))
  expect_error(add_object_keyword(time = letters[1:5]))
})

test_that("add_batch5", {
  expect_error(add_control_keyword(keyword = list(letters[1:6])))

  expect_error(add_object_keyword(keyword = list(letters[1:5])))
})

# add_synonyms ----
test_that("add_synonym1", {
  expect_error(add_synonym(keyword = letters[1:2], synonym = LETTERS[1:2]))
})

test_that("add_synonym2", {
  expect_error(add_synonym(keyword = 1, synonym = "A"))
  expect_error(add_synonym(keyword = TRUE, synonym = "A"))
  expect_error(add_synonym(keyword = sum, synonym = "A"))
})

test_that("add_synonym3", {
  expect_error(add_synonym(keyword = "A", synonym = 1))
  expect_error(add_synonym(keyword = "A", synonym = TRUE))
  expect_error(add_synonym(keyword = "A", synonym = sum))
})

# download_control ----
test_that("download_control1", {
  expect_error(download_control(control = 1.5))
  expect_error(download_control(control = "A"))
  expect_error(download_control(control = TRUE))
  expect_error(download_control(control = sum))
})

test_that("download_control2", {
  expect_error(download_control(locations = 1))
  expect_error(download_control(locations = TRUE))
  expect_error(download_control(locations = sum))
})

test_that("download_control3", {
  expect_error(download_control_global(control = 1.5))
  expect_error(download_control_global(control = "A"))
  expect_error(download_control_global(control = TRUE))
  expect_error(download_control_global(control = sum))
})

# download_object ----
test_that("download_object1", {
  expect_error(download_object(object = 1.5))
  expect_error(download_object(object = "A"))
  expect_error(download_object(object = TRUE))
  expect_error(download_object(object = sum))
})

test_that("download_object2", {
  expect_error(download_object(control = 1.5, object = 1))
  expect_error(download_object(control = "A", object = 1))
  expect_error(download_object(control = TRUE, object = 1))
  expect_error(download_object(control = sum, object = 1))
  expect_error(download_object(control = 1:5, object = 1))
})

test_that("download_object3", {
  expect_error(download_object(object = 1, locations = 1))
  expect_error(download_object(object = 1, locations = TRUE))
  expect_error(download_object(object = 1, locations = sum))
})

test_that("download_object4", {
  expect_error(download_object_global(object = 1.5))
  expect_error(download_object_global(object = "A"))
  expect_error(download_object_global(object = TRUE))
  expect_error(download_object_global(object = sum))
})

test_that("download_object5", {
  expect_error(download_object_global(control = 1.5, object = 1))
  expect_error(download_object_global(control = "A", object = 1))
  expect_error(download_object_global(control = TRUE, object = 1))
  expect_error(download_object_global(control = sum, object = 1))
  expect_error(download_object_global(control = 1:5, object = 1))
})

# compute_score ----
test_that("compute_score1", {
  expect_error(compute_score(object = 1.5))
  expect_error(compute_score(object = "A"))
  expect_error(compute_score(object = TRUE))
  expect_error(compute_score(object = sum))
})

test_that("compute_score2", {
  expect_error(compute_score(control = 1.5, object = 1))
  expect_error(compute_score(control = "A", object = 1))
  expect_error(compute_score(control = sum, object = 1))
  expect_error(compute_score(control = 1:5, object = 1))
})

test_that("compute_score3", {
  expect_error(compute_score(object = 1, locations = 1))
  expect_error(compute_score(object = 1, locations = TRUE))
  expect_error(compute_score(object = 1, locations = sum))
})

test_that("compute_score4", {
  expect_error(compute_score_global(object = 1.5))
  expect_error(compute_score_global(object = "A"))
  expect_error(compute_score_global(object = TRUE))
  expect_error(compute_score_global(object = sum))
})

test_that("compute_score5", {
  expect_error(compute_score_global(control = 1.5, object = 1))
  expect_error(compute_score_global(control = "A", object = 1))
  expect_error(compute_score_global(control = sum, object = 1))
  expect_error(compute_score_global(control = 1:5, object = 1))
})

# compute_doi ----
test_that("compute_doi1", {
  expect_error(compute_doi(object = 1.5))
  expect_error(compute_doi(object = "A"))
  expect_error(compute_doi(object = TRUE))
  expect_error(compute_doi(object = sum))
})

test_that("compute_doi2", {
  expect_error(compute_doi(control = 1.5, object = 1))
  expect_error(compute_doi(control = "A", object = 1))
  expect_error(compute_doi(control = sum, object = 1))
  expect_error(compute_doi(control = 1:5, object = 1))
})

test_that("compute_doi3", {
  expect_error(compute_doi(object = 1, locations = 1))
  expect_error(compute_doi(object = 1, locations = TRUE))
  expect_error(compute_doi(object = 1, locations = sum))
})

test_that("compute_doi4", {
  expect_error(compute_doi(object = 1, locations = letters[1:2]))
})

# export_data ----
test_that("export_data1a", {
  expect_error(export_control(control = 1.5))
  expect_error(export_control(control = "A"))
  expect_error(export_control(control = TRUE))
  expect_error(export_control(control = sum))
  expect_error(export_control(control = 1:5))
})

test_that("export_data1b", {
  expect_error(export_control_global(control = 1.5))
  expect_error(export_control_global(control = "A"))
  expect_error(export_control_global(control = TRUE))
  expect_error(export_control_global(control = sum))
  expect_error(export_control_global(control = 1:5))
})
test_that("export_data2a", {
  expect_error(export_object(keyword = 1))
  expect_error(export_object(keyword = TRUE))
  expect_error(export_object(keyword = sum))
  expect_error(export_object(keyword = letters[1:5]))
})

test_that("export_data2b", {
  expect_error(export_object(object = 1.5))
  expect_error(export_object(object = "A"))
  expect_error(export_object(object = TRUE))
  expect_error(export_object(object = sum))
  expect_error(export_object(object = 1:5))
})

test_that("export_data2c", {
  expect_error(export_object(control = 1.5))
  expect_error(export_object(control = "A"))
  expect_error(export_object(control = TRUE))
  expect_error(export_object(control = sum))
  expect_error(export_object(control = 1:5))
})

test_that("export_data2d", {
  expect_error(export_object_global(keyword = 1))
  expect_error(export_object_global(keyword = TRUE))
  expect_error(export_object_global(keyword = sum))
  expect_error(export_object_global(keyword = letters[1:5]))
})

test_that("export_data2e", {
  expect_error(export_object_global(object = 1.5))
  expect_error(export_object_global(object = "A"))
  expect_error(export_object_global(object = TRUE))
  expect_error(export_object_global(object = sum))
  expect_error(export_object_global(object = 1:5))
})

test_that("export_data2f", {
  expect_error(export_object_global(control = 1.5))
  expect_error(export_object_global(control = "A"))
  expect_error(export_object_global(control = TRUE))
  expect_error(export_object_global(control = sum))
  expect_error(export_object_global(control = 1:5))
})

test_that("export_data3a", {
  expect_error(export_score(keyword = 1))
  expect_error(export_score(keyword = TRUE))
  expect_error(export_score(keyword = sum))
  expect_error(export_score(keyword = letters[1:5]))
})

test_that("export_data3b", {
  expect_error(export_score(object = 1.5))
  expect_error(export_score(object = "A"))
  expect_error(export_score(object = TRUE))
  expect_error(export_score(object = sum))
  expect_error(export_score(object = 1:5))
})

test_that("export_data3c", {
  expect_error(export_score(control = 1.5))
  expect_error(export_score(control = "A"))
  expect_error(export_score(control = TRUE))
  expect_error(export_score(control = sum))
  expect_error(export_score(control = 1:5))
})

test_that("export_data3d", {
  expect_error(export_voi(keyword = 1))
  expect_error(export_voi(keyword = TRUE))
  expect_error(export_voi(keyword = sum))
  expect_error(export_voi(keyword = letters[1:5]))
})

test_that("export_data3e", {
  expect_error(export_voi(object = 1.5))
  expect_error(export_voi(object = "A"))
  expect_error(export_voi(object = TRUE))
  expect_error(export_voi(object = sum))
  expect_error(export_voi(object = 1:5))
})

test_that("export_data3f", {
  expect_error(export_voi(control = 1.5))
  expect_error(export_voi(control = "A"))
  expect_error(export_voi(control = TRUE))
  expect_error(export_voi(control = sum))
  expect_error(export_voi(control = 1:5))
})

test_that("export_data4a", {
  expect_error(export_doi(keyword = 1))
  expect_error(export_doi(keyword = TRUE))
  expect_error(export_doi(keyword = sum))
  expect_error(export_doi(keyword = letters[1:5]))
})

test_that("export_data4b", {
  expect_error(export_doi(object = 1.5))
  expect_error(export_doi(object = "A"))
  expect_error(export_doi(object = TRUE))
  expect_error(export_doi(object = sum))
  expect_error(export_doi(object = 1:5))
})

test_that("export_data4c", {
  expect_error(export_doi(control = 1.5))
  expect_error(export_doi(control = "A"))
  expect_error(export_doi(control = TRUE))
  expect_error(export_doi(control = sum))
  expect_error(export_doi(control = 1:5))
})

test_that("export_data4d", {
  expect_error(export_doi(type = 1))
  expect_error(export_doi(type = "A"))
  expect_error(export_doi(type = TRUE))
  expect_error(export_doi(type = sum))
  expect_error(export_doi(type = c("obs", "sad", "trd")))
})

test_that("export_data4e", {
  expect_error(export_doi(locations = 1))
  expect_error(export_doi(locations = TRUE))
  expect_error(export_doi(locations = sum))
  expect_error(export_doi(locations = letters[1:5]))
})

# plot_score ----
test_that("plot_score1", {
  expect_error(plot_score(data_score = 1))
  expect_error(plot_score(data_score = "A"))
  expect_error(plot_score(data_score = TRUE))
  expect_error(plot_score(data_score = sum))
})

test_that("plot_score2", {
  data <- export_score(keyword = "fc barcelona")
  expect_error(plot_score(data_score = data, type = 1))
  expect_error(plot_score(data_score = data, type = "A"))
  expect_error(plot_score(data_score = data, type = TRUE))
  expect_error(plot_score(data_score = data, type = sum))
  expect_error(plot_score(data_score = data, type = c("obs", "sad", "trd")))
})

test_that("plot_score3", {
  data <- export_score()
  expect_warning(plot_score(data_score = data))
})

# plot_doi_ts ----
test_that("plot_doi_ts1", {
  expect_error(plot_doi_ts(data_doi = 1))
  expect_error(plot_doi_ts(data_doi = "A"))
  expect_error(plot_doi_ts(data_doi = TRUE))
  expect_error(plot_doi_ts(data_doi = sum))
})

test_that("plot_doi_ts2", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_ts(data_doi = data, type = 1))
  expect_error(plot_doi_ts(data_doi = data, type = "A"))
  expect_error(plot_doi_ts(data_doi = data, type = TRUE))
  expect_error(plot_doi_ts(data_doi = data, type = sum))
  expect_error(plot_doi_ts(data_doi = data, type = c("obs", "sad", "trd")))
})

test_that("plot_doi_ts3", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_ts(data_doi = data, measure = 1))
  expect_error(plot_doi_ts(data_doi = data, measure = "A"))
  expect_error(plot_doi_ts(data_doi = data, measure = TRUE))
  expect_error(plot_doi_ts(data_doi = data, measure = sum))
  expect_error(plot_doi_ts(data_doi = data, measure = c("gini", "hhi", "entropy")))
})

test_that("plot_doi_ts4", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_ts(data_doi = data, locations = 1))
  expect_error(plot_doi_ts(data_doi = data, locations = TRUE))
  expect_error(plot_doi_ts(data_doi = data, locations = sum))
  expect_error(plot_doi_ts(data_doi = data, locations = letters[1:5]))
})

test_that("plot_doi_ts5", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_ts(data_doi = data, smooth = 1))
  expect_error(plot_doi_ts(data_doi = data, smooth = "A"))
  expect_error(plot_doi_ts(data_doi = data, smooth = sum))
  expect_error(plot_doi_ts(data_doi = data, smooth = c(TRUE, TRUE)))
})

test_that("plot_doi_ts6", {
  data <- export_doi()
  expect_warning(plot_doi_ts(data_doi = data))
})

# plot_voi_ts ----
test_that("plot_voi_ts1", {
  expect_error(plot_voi_ts(data_voi = 1))
  expect_error(plot_voi_ts(data_voi = "A"))
  expect_error(plot_voi_ts(data_voi = TRUE))
  expect_error(plot_voi_ts(data_voi = sum))
})

test_that("plot_voi_ts2", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_ts(data_voi = data, type = 1))
  expect_error(plot_voi_ts(data_voi = data, type = "A"))
  expect_error(plot_voi_ts(data_voi = data, type = TRUE))
  expect_error(plot_voi_ts(data_voi = data, type = sum))
  expect_error(plot_voi_ts(data_voi = data, type = c("obs", "sad", "trd")))
})

test_that("plot_voi_ts3", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_ts(data_voi = data, smooth = 1))
  expect_error(plot_voi_ts(data_voi = data, smooth = "A"))
  expect_error(plot_voi_ts(data_voi = data, smooth = sum))
  expect_error(plot_voi_ts(data_voi = data, smooth = c(TRUE, TRUE)))
})

test_that("plot_voi_ts4", {
  data <- export_voi()
  expect_warning(plot_voi_ts(data_voi = data))
})

# plot_doi_box ----
test_that("plot_doi_box1", {
  expect_error(plot_doi_box(data_doi = 1))
  expect_error(plot_doi_box(data_doi = "A"))
  expect_error(plot_doi_box(data_doi = TRUE))
  expect_error(plot_doi_box(data_doi = sum))
})

test_that("plot_doi_box2", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_box(data_doi = data, type = 1))
  expect_error(plot_doi_box(data_doi = data, type = "A"))
  expect_error(plot_doi_box(data_doi = data, type = TRUE))
  expect_error(plot_doi_box(data_doi = data, type = sum))
  expect_error(plot_doi_box(data_doi = data, type = c("obs", "sad", "trd")))
})

test_that("plot_doi_box3", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_box(data_doi = data, measure = 1))
  expect_error(plot_doi_box(data_doi = data, measure = "A"))
  expect_error(plot_doi_box(data_doi = data, measure = TRUE))
  expect_error(plot_doi_box(data_doi = data, measure = sum))
  expect_error(plot_doi_box(data_doi = data, measure = c("gini", "hhi", "entropy")))
})

test_that("plot_doi_box4", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(plot_doi_box(data_doi = data, locations = 1))
  expect_error(plot_doi_box(data_doi = data, locations = TRUE))
  expect_error(plot_doi_box(data_doi = data, locations = sum))
  expect_error(plot_doi_box(data_doi = data, locations = letters[1:5]))
})

test_that("plot_doi_box5", {
  data <- export_doi()
  expect_warning(plot_doi_box(data_doi = data))
})

# plot_voi_box ----
test_that("plot_voi_box1", {
  expect_error(plot_voi_box(data_voi = 1))
  expect_error(plot_voi_box(data_voi = "A"))
  expect_error(plot_voi_box(data_voi = TRUE))
  expect_error(plot_voi_box(data_voi = sum))
})

test_that("plot_voi_box2", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_box(data_voi = data, type = 1))
  expect_error(plot_voi_box(data_voi = data, type = "A"))
  expect_error(plot_voi_box(data_voi = data, type = TRUE))
  expect_error(plot_voi_box(data_voi = data, type = sum))
  expect_error(plot_voi_box(data_voi = data, type = c("obs", "sad", "trd")))
})

test_that("plot_voi_box4", {
  data <- export_voi()
  expect_warning(plot_voi_box(data_voi = data))
})

# plot_voi_doi ----
test_that("plot_voi_doi1", {
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_doi(data_doi = 1, data_score_global = data2))
  expect_error(plot_voi_doi(data_doi = "A", data_score_global = data2))
  expect_error(plot_voi_doi(data_doi = TRUE, data_score_global = data2))
  expect_error(plot_voi_doi(data_doi = sum, data_score_global = data2))
})

test_that("plot_voi_doi2", {
  data1 <- export_doi(keyword = "fc barcelona")
  expect_error(plot_voi_doi(data_score_global = 1, data_doi = data1))
  expect_error(plot_voi_doi(data_score_global = "A", data_doi = data1))
  expect_error(plot_voi_doi(data_score_global = TRUE, data_doi = data1))
  expect_error(plot_voi_doi(data_score_global = sum, data_doi = data1))
})

test_that("plot_voi_doi3", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, type = 1))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, type = "A"))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, type = TRUE))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, type = sum))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, type = c("obs", "sad", "trd")))
})

test_that("plot_voi_doi4", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, measure = 1))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, measure = "A"))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, measure = TRUE))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, measure = sum))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, measure = c("gini", "hhi", "entropy")))
})

test_that("plot_voi_doi5", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, locations = 1))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, locations = TRUE))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, locations = sum))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, locations = letters[1:5]))
})

test_that("plot_voi_doi6", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, smooth = 1))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, smooth = "A"))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, smooth = sum))
  expect_error(plot_voi_doi(data_doi = data1, data_score_global = data2, smooth = c(TRUE, TRUE)))
})

test_that("plot_voi_doi7", {
  data1 <- export_doi()
  data2 <- export_voi()
  expect_warning(plot_voi_doi(data_doi = data1, data_score_global = data2))
})

# remove_data ----
test_that("remove_data1", {
  expect_error(remove_data(table = 1))
  expect_error(remove_data(table = "A"))
  expect_error(remove_data(table = TRUE))
  expect_error(remove_data(table = sum))
  expect_error(remove_data(table = c("data_object", "data_control")))
})

test_that("remove_data2", {
  expect_error(remove_data(table = "data_object", control = 1.5, object = 1))
  expect_error(remove_data(table = "data_object", control = "A", object = 1))
  expect_error(remove_data(table = "data_object", control = sum, object = 1))
  expect_error(remove_data(table = "data_object", control = 1:5, object = 1))
})

test_that("remove_data3", {
  expect_error(remove_data(table = "data_object", object = 1.5, control = 1))
  expect_error(remove_data(table = "data_object", object = "A", control = 1))
  expect_error(remove_data(table = "data_object", object = sum, control = 1))
  expect_error(remove_data(table = "data_object", object = 1:5, control = 1))
})

# disconnect ----
disconnect_db()
unlink("db", recursive = TRUE)
