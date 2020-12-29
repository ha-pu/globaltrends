# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
data <- filter(data_control, batch == 1 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(data_object, batch_c == 1 & batch_o %in% 1:3 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)
data <- filter(data_score, batch_c == 1 & batch_o %in% 1:3 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_score", data, append = TRUE)
data <- filter(data_doi, batch_c == 1 & batch_o %in% 1:3 & locations == "countries")
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

# export_data ------------------------------------------------------------------
test_that("export_data1a", {
  expect_error(
    export_control(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_control(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_control(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_control(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_control(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data1b", {
  expect_error(
    export_control_global(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_control_global(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_control_global(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_control_global(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_control_global(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data2a", {
  expect_error(
    export_object(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_object(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_object(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_object(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data2b", {
  expect_error(
    export_object(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_object(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data2c", {
  expect_error(
    export_object(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_object(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data2d", {
  expect_error(
    export_object_global(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_object_global(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_object_global(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_object_global(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data2e", {
  expect_error(
    export_object_global(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_object_global(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object_global(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object_global(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object_global(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data2f", {
  expect_error(
    export_object_global(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_object_global(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object_global(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object_global(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_object_global(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data3a", {
  expect_error(
    export_score(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_score(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_score(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_score(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data3b", {
  expect_error(
    export_score(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_score(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_score(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_score(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_score(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data3c", {
  expect_error(
    export_score(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_score(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_score(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_score(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_score(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data3d", {
  expect_error(
    export_voi(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_voi(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_voi(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_voi(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data3e", {
  expect_error(
    export_voi(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_voi(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data3f", {
  expect_error(
    export_voi(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_voi(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data4a", {
  expect_error(
    export_doi(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data4b", {
  expect_error(
    export_doi(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_doi(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data4c", {
  expect_error(
    export_doi(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_doi(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_data4d", {
  expect_error(
    export_doi(type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi(type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    export_doi(type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi(type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi(type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("export_data4e", {
  expect_error(
    export_doi(locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi(locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi(locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi(locations = letters[1:5]),
    "'locations' must be object of length 1.\nYou provided an object of length 5."
  )
})

# export_change ------------------------------------------------------------------
test_that("export_change1a", {
  expect_error(
    export_voi_change(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_voi_change(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi_change(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi_change(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi_change(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change1b", {
  expect_error(
    export_doi_change(control = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_doi_change(control = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi_change(control = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi_change(control = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi_change(control = 1:5),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change2a", {
  expect_error(
    export_voi_change(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_voi_change(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_voi_change(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_voi_change(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change2b", {
  expect_error(
    export_doi_change(keyword = 1),
    "'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi_change(keyword = TRUE),
    "'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi_change(keyword = sum),
    "'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi_change(keyword = letters[1:5]),
    "'keyword' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change3a", {
  expect_error(
    export_voi_change(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_voi_change(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi_change(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi_change(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_voi_change(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change3b", {
  expect_error(
    export_doi_change(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    export_doi_change(object = "A"),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi_change(object = TRUE),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi_change(object = sum),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    export_doi_change(object = 1:5),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change4a", {
  expect_error(
    export_voi_change(type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_voi_change(type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    export_doi_change(type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_voi_change(type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_voi_change(type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("export_change4b", {
  expect_error(
    export_doi_change(type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi_change(type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    export_doi_change(type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi_change(type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi_change(type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("export_change5", {
  expect_error(
    export_doi_change(locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi_change(locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi_change(locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi_change(locations = letters[1:5]),
    "'locations' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("export_change6", {
  expect_error(
    export_doi_change(measure = 1),
    "'measure' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi_change(measure = "A"),
    "'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou provided A."
  )
  expect_error(
    export_doi_change(measure = TRUE),
    "'measure' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi_change(measure = sum),
    "'measure' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    export_doi_change(measure = c("gini", "hhi", "entropy")),
    "'measure' must be object of length 1.\nYou provided an object of length 3."
  )
})

# plot_score -------------------------------------------------------------------
test_that("plot_score1", {
  expect_error(
    plot_score(data_score = 1),
    "'data_score' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_score(data_score = "A"),
    "'data_score' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_score(data_score = TRUE),
    "'data_score' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_score(data_score = sum),
    "'data_score' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_score2", {
  data <- export_score(keyword = "fc barcelona")
  expect_error(
    plot_score(data_score = data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_score(data_score = data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_score(data_score = data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_score(data_score = data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_score(data_score = data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_score3", {
  data <- export_score()
  expect_warning(
    plot_score(data_score = data),
    "The plot function is limited to 1 keyword.\nYou use 8 keywords.\nOnly the first keyword is used."
  )
})

# plot_doi_ts ------------------------------------------------------------------
test_that("plot_doi_ts1", {
  expect_error(
    plot_doi_ts(data_doi = 1),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_ts(data_doi = "A"),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_doi_ts(data_doi = TRUE),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_ts(data_doi = sum),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_doi_ts2", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_ts(data_doi = data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_ts(data_doi = data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_doi_ts(data_doi = data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_ts(data_doi = data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_ts(data_doi = data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_doi_ts3", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_ts(data_doi = data, measure = 1),
    "'measure' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_ts(data_doi = data, measure = "A"),
    "'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou provided A."
  )
  expect_error(
    plot_doi_ts(data_doi = data, measure = TRUE),
    "'measure' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_ts(data_doi = data, measure = sum),
    "'measure' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_ts(data_doi = data, measure = c("gini", "hhi", "entropy")),
    "'measure' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_doi_ts4", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_ts(data_doi = data, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_ts(data_doi = data, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_ts(data_doi = data, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_ts(data_doi = data, locations = letters[1:5]),
    "'locations' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("plot_doi_ts5", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_ts(data_doi = data, smooth = 1),
    "'smooth' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_ts(data_doi = data, smooth = "A"),
    "'smooth' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    plot_doi_ts(data_doi = data, smooth = sum),
    "'smooth' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_ts(data_doi = data, smooth = c(TRUE, TRUE)),
    "'smooth' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("plot_doi_ts6", {
  data <- export_doi()
  expect_warning(
    plot_doi_ts(data_doi = data),
    "The plot function is limited to 9 keywords in a grid."
  )
})

# plot_voi_ts ------------------------------------------------------------------
test_that("plot_voi_ts1", {
  expect_error(
    plot_voi_ts(data_voi = 1),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_ts(data_voi = "A"),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_ts(data_voi = TRUE),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_ts(data_voi = sum),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_voi_ts2", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_ts(data_voi = data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_ts(data_voi = data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_voi_ts(data_voi = data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_ts(data_voi = data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_ts(data_voi = data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_voi_ts3", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_ts(data_voi = data, smooth = 1),
    "'smooth' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_ts(data_voi = data, smooth = "A"),
    "'smooth' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_ts(data_voi = data, smooth = sum),
    "'smooth' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_ts(data_voi = data, smooth = c(TRUE, TRUE)),
    "'smooth' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("plot_voi_ts4", {
  data <- export_voi()
  expect_warning(
    plot_voi_ts(data_voi = data),
    "The plot function is limited to 9 keywords in a grid."
  )
})

# plot_doi_box -----------------------------------------------------------------
test_that("plot_doi_box1", {
  expect_error(
    plot_doi_box(data_doi = 1),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_box(data_doi = "A"),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_doi_box(data_doi = TRUE),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_box(data_doi = sum),
    "'data_doi' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_doi_box2", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_box(data_doi = data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_box(data_doi = data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_doi_box(data_doi = data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_box(data_doi = data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_box(data_doi = data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_doi_box3", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_box(data_doi = data, measure = 1),
    "'measure' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_box(data_doi = data, measure = "A"),
    "'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou provided A."
  )
  expect_error(
    plot_doi_box(data_doi = data, measure = TRUE),
    "'measure' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_box(data_doi = data, measure = sum),
    "'measure' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_box(data_doi = data, measure = c("gini", "hhi", "entropy")),
    "'measure' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_doi_box4", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_doi_box(data_doi = data, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_box(data_doi = data, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_box(data_doi = data, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_box(data_doi = data, locations = letters[1:5]),
    "'locations' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("plot_doi_box5", {
  data <- export_doi()
  expect_warning(
    plot_doi_box(data_doi = data),
    "The plot function is limited to 9 keywords in a boxplot."
  )
})

# plot_voi_box -----------------------------------------------------------------
test_that("plot_voi_box1", {
  expect_error(
    plot_voi_box(data_voi = 1),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_box(data_voi = "A"),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_box(data_voi = TRUE),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_box(data_voi = sum),
    "'data_voi' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_voi_box2", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_voi_box(data_voi = data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_box(data_voi = data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_voi_box(data_voi = data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_box(data_voi = data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_box(data_voi = data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_voi_box4", {
  data <- export_voi()
  expect_warning(
    plot_voi_box(data_voi = data),
    "The plot function is limited to 9 keywords in a boxplot."
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

# plot_change ------------------------------------------------------------------
test_that("plot_change1a", {
  expect_error(
    plot_voi_change(data_change = 1),
    "'data_change' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_change(data_change = "A"),
    "'data_change' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_change(data_change = TRUE),
    "'data_change' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_change(data_change = sum),
    "'data_change' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_change1b", {
  expect_error(
    plot_voi_change(data_change = 1),
    "'data_change' must be object of type data.frame.\nYou provided an object of type double."
  )
  expect_error(
    plot_voi_change(data_change = "A"),
    "'data_change' must be object of type data.frame.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_change(data_change = TRUE),
    "'data_change' must be object of type data.frame.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_change(data_change = sum),
    "'data_change' must be object of type data.frame.\nYou provided an object of type builtin."
  )
})

test_that("plot_change2", {
  data <- export_doi_change(keyword = "fc barcelona")
  expect_error(
    plot_doi_change(data_change = data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_doi_change(data_change = data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_doi_change(data_change = data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_change(data_change = data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_change(data_change = data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_change3a", {
  data <- export_voi_change(keyword = "fc barcelona")
  expect_error(
    plot_voi_change(data_change = data, ci = "A"),
    "'ci' must be object of type double.\nYou provided an object of type character."
  )
  expect_error(
    plot_voi_change(data_change = data, ci = TRUE),
    "'ci' must be object of type double.\nYou provided an object of type logical."
  )
  expect_error(
    plot_voi_change(data_change = data, ci = sum),
    "'ci' must be object of type double.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_voi_change(data_change = data, ci = 1:3 / 10),
    "'ci' must be object of length 1.\nYou provided an object of length 3."
  )
  expect_error(
    plot_voi_change(data_change = data, ci = 0),
    "'ci' must be greater than 0 and less than 1.\nYou provided 0."
  )
  expect_error(
    plot_voi_change(data_change = data, ci = 1),
    "'ci' must be greater than 0 and less than 1.\nYou provided 1."
  )
})

test_that("plot_change3b", {
  data <- export_doi_change(keyword = "fc barcelona")
  expect_error(
    plot_doi_change(data_change = data, ci = "A"),
    "'ci' must be object of type double.\nYou provided an object of type character."
  )
  expect_error(
    plot_doi_change(data_change = data, ci = TRUE),
    "'ci' must be object of type double.\nYou provided an object of type logical."
  )
  expect_error(
    plot_doi_change(data_change = data, ci = sum),
    "'ci' must be object of type double.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_doi_change(data_change = data, ci = 1:3 / 10),
    "'ci' must be object of length 1.\nYou provided an object of length 3."
  )
  expect_error(
    plot_doi_change(data_change = data, ci = 0),
    "'ci' must be greater than 0 and less than 1.\nYou provided 0."
  )
  expect_error(
    plot_doi_change(data_change = data, ci = 1),
    "'ci' must be greater than 0 and less than 1.\nYou provided 1."
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
