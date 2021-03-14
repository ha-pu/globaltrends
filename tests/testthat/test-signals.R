# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
data <- filter(example_control, batch == 1 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o %in% 1:3 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)
data <- filter(example_score, batch_c == 1 & batch_o %in% 1:3 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_score", data, append = TRUE)
data <- filter(example_doi, batch_c == 1 & batch_o %in% 1:3 & locations == "countries")
dbWriteTable(globaltrends_db, "data_doi", data, append = TRUE)

# add_control / add_keyword ----------------------------------------------------
test_that("add_batch1", {
  expect_error(
    add_control_keyword(keyword = 1),
    "no applicable method"
  )
  expect_error(
    add_control_keyword(keyword = TRUE),
    "no applicable method"
  )
  expect_error(
    add_control_keyword(keyword = sum),
    "no applicable method"
  )
})

test_that("add_batch2", {
  expect_error(
    add_control_keyword(time = 1),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(time = TRUE),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(time = sum),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(time = letters[1:5]),
    '"keyword"'
  )
})

test_that("add_batch3", {
  expect_error(
    add_object_keyword(keyword = 1),
    "no applicable method"
  )
  expect_error(
    add_object_keyword(keyword = TRUE),
    "no applicable method"
  )
  expect_error(
    add_object_keyword(keyword = sum),
    "no applicable method"
  )
})

test_that("add_batch4", {
  expect_error(
    add_object_keyword(time = 1),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(time = TRUE),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(time = sum),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(time = letters[1:5]),
    '"keyword"'
  )
})

test_that("add_batch5", {
  expect_error(
    add_control_keyword(keyword = list(letters[1:6])),
    "'keyword' must be object of length 5.\nYou provided an object of length 6."
  )

  expect_error(
    add_object_keyword(keyword = list(letters[1:5])),
    "'keyword' must be object of length 4.\nYou provided an object of length 5."
  )
})

# add_synonyms -----------------------------------------------------------------
test_that("add_synonym1", {
  expect_error(
    add_synonym(keyword = letters[1:2], synonym = LETTERS[1:2]),
    "'keyword' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("add_synonym2", {
  expect_error(
    add_synonym(keyword = 1, synonym = "A"),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_synonym(keyword = TRUE, synonym = "A"),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_synonym(keyword = sum, synonym = "A"),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("add_synonym3", {
  expect_error(
    add_synonym(keyword = "A", synonym = 1),
    "no applicable method"
  )
  expect_error(
    add_synonym(keyword = "A", synonym = TRUE),
    "no applicable method"
  )
  expect_error(
    add_synonym(keyword = "A", synonym = sum),
    "no applicable method"
  )
})

# download_control -------------------------------------------------------------
test_that("download_control1", {
  expect_error(
    download_control(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    download_control(control = "A"),
    "no applicable method"
  )
  expect_error(
    download_control(control = TRUE),
    "no applicable method"
  )
  expect_error(
    download_control(control = sum),
    "no applicable method"
  )
})

test_that("download_control2", {
  expect_error(
    download_control(locations = 1),
    '"control"'
  )
  expect_error(
    download_control(locations = TRUE),
    '"control"'
  )
  expect_error(
    download_control(locations = sum),
    '"control"'
  )
})

test_that("download_control3", {
  expect_error(
    download_control_global(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    download_control_global(control = "A"),
    "no applicable method"
  )
  expect_error(
    download_control_global(control = TRUE),
    "no applicable method"
  )
  expect_error(
    download_control_global(control = sum),
    "no applicable method"
  )
})

# download_object --------------------------------------------------------------
test_that("download_object1", {
  expect_error(
    download_object(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    download_object(object = "A"),
    "no applicable method"
  )
  expect_error(
    download_object(object = TRUE),
    "no applicable method"
  )
  expect_error(
    download_object(object = sum),
    "no applicable method"
  )
})

test_that("download_object2", {
  expect_message(
    download_object(control = 1, object = 1, location = "JP"),
    "Download for objec data failed.\nThere is no data in 'data_control' for control batch 1 and location JP."
  )
  expect_error(
    download_object(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    download_object(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    download_object(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    download_object(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    download_object(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("download_object3", {
  expect_error(
    download_object(object = 1, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    download_object(object = 1, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    download_object(object = 1, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("download_object4", {
  expect_error(
    download_object_global(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    download_object_global(object = "A"),
    "no applicable method"
  )
  expect_error(
    download_object_global(object = TRUE),
    "no applicable method"
  )
  expect_error(
    download_object_global(object = sum),
    "no applicable method"
  )
})

test_that("download_object5", {
  expect_error(
    download_object_global(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    download_object_global(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    download_object_global(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    download_object_global(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    download_object_global(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

# compute_score ----------------------------------------------------------------
test_that("compute_score1", {
  expect_error(
    compute_score(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_score(object = "A"),
    "no applicable method"
  )
  expect_error(
    compute_score(object = TRUE),
    "no applicable method"
  )
  expect_error(
    compute_score(object = sum),
    "no applicable method"
  )
})

test_that("compute_score2", {
  expect_error(
    compute_score(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_score(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_score(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_score(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_score(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("compute_score3", {
  expect_error(
    compute_score(object = 1, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_score(object = 1, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    compute_score(object = 1, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("compute_score4", {
  expect_error(
    compute_voi(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_voi(object = "A"),
    "no applicable method"
  )
  expect_error(
    compute_voi(object = TRUE),
    "no applicable method"
  )
  expect_error(
    compute_voi(object = sum),
    "no applicable method"
  )
})

test_that("compute_score5", {
  expect_error(
    compute_voi(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_voi(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_voi(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_voi(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_voi(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

# compute_doi ------------------------------------------------------------------
test_that("compute_doi1", {
  expect_error(
    compute_doi(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_doi(object = "A"),
    "no applicable method"
  )
  expect_error(
    compute_doi(object = TRUE),
    "no applicable method"
  )
  expect_error(
    compute_doi(object = sum),
    "no applicable method"
  )
})

test_that("compute_doi2", {
  expect_error(
    compute_doi(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_doi(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_doi(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_doi(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_doi(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("compute_doi3", {
  expect_error(
    compute_doi(object = 1, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_doi(object = 1, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical"
  )
  expect_error(
    compute_doi(object = 1, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin"
  )
})

test_that("compute_doi4", {
  expect_error(
    compute_doi(object = 1, locations = letters[1:2]),
    "'locations' must be object of length 1.\nYou provided an object of length 2."
  )
})

# plot_voi_doi -----------------------------------------------------------------
test_that("plot_voi_doi1", {
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_doi(data_doi = 1, data_voi = data2),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_doi(data_doi = "A", data_voi = data2),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_doi(data_doi = TRUE, data_voi = data2),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_doi(data_doi = sum, data_voi = data2),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_voi_doi2", {
  data1 <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_voi_doi(data_voi = 1, data_doi = data1),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_doi(data_voi = "A", data_doi = data1),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_doi(data_voi = TRUE, data_doi = data1),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_doi(data_voi = sum, data_doi = data1),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_voi_doi3", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_voi_doi4", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, measure = 1),
    "'measure' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, measure = "A"),
    "'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou provided A."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, measure = TRUE),
    "'measure' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, measure = sum),
    "'measure' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, measure = c("gini", "hhi", "entropy")),
    "'measure' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_voi_doi5", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, locations = letters[1:5]),
    "'locations' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("plot_voi_doi6", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, smooth = 1),
    "'smooth' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, smooth = "A"),
    "'smooth' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, smooth = sum),
    "'smooth' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_doi(data_doi = data1, data_voi = data2, smooth = c(TRUE, TRUE)),
    "'smooth' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("plot_voi_doi7", {
  data1 <- export_voi()
  data2 <- export_doi()
  expect_warning(
    plot_voi_doi(data_voi = data1, data_doi = data2),
    "The plot function is limited to 1 keyword.\nYou use 10 keywords.\nOnly the first keyword is used."
  )
})

# remove_data ------------------------------------------------------------------
test_that("remove_data1", {
  expect_error(
    remove_data(table = 1),
    "'table' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    remove_data(table = "A"),
    "'table' must be either 'batch_keywords', 'batch_time', 'data_control', 'data_object', 'data_score', or 'data_doi'.\nYou provided A."
  )
  expect_error(
    remove_data(table = TRUE),
    "'table' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    remove_data(table = sum),
    "'table' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    remove_data(table = c("data_object", "data_control")),
    "'table' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("remove_data2", {
  expect_error(
    remove_data(table = "data_object", control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    remove_data(table = "data_object", control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("remove_data3", {
  expect_error(
    remove_data(table = "data_object", object = 1.5, control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    remove_data(table = "data_object", object = "A", control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", object = TRUE, control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", object = sum, control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", object = 1:5, control = 1),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
