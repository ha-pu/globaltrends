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

# export_control ---------------------------------------------------------------
test_that("export_control1", {
  out <- export_control(control = 1)
  expect_equal(nrow(out), 1200)
})

test_that("export_control2", {
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

# export_control_global --------------------------------------------------------
test_that("export_control_global1", {
  out <- export_control_global(control = 1)
  expect_equal(nrow(out), 600)
})

test_that("export_control_global2", {
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

# export_object ----------------------------------------------------------------
test_that("export_object1", {
  out <- export_object(keyword = "manchester united")
  expect_equal(nrow(out), 240)
})

test_that("export_object2", {
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

test_that("export_object3", {
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

test_that("export_object4", {
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

# export_object_global ---------------------------------------------------------
test_that("export_object_global1", {
  out <- export_object_global(keyword = "manchester united")
  expect_equal(nrow(out), 120)
})

test_that("export_object_global2", {
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

test_that("export_object_global3", {
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

test_that("export_object_global4", {
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

# export_score -----------------------------------------------------------------
test_that("export_score1", {
  out <- export_score(keyword = "real madrid")
  expect_equal(nrow(out), 240)
  expect_s3_class(out, "exp_score")
})

test_that("export_score2", {
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

test_that("export_score3", {
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

test_that("export_score4", {
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

# export_voi -------------------------------------------------------------------
test_that("export_voi1", {
  out <- export_voi(keyword = "real madrid")
  expect_equal(nrow(out), 120)
  expect_s3_class(out, "exp_voi")
})

test_that("export_voi2", {
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

test_that("export_voi3", {
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

test_that("export_voi4", {
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

# export_doi -------------------------------------------------------------------
test_that("export_doi1", {
  out <- export_doi(control = 1, object = 1, type = "trd", locations = "countries")
  expect_equal(nrow(out), 480)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi2", {
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

test_that("export_doi3", {
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

test_that("export_doi4", {
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

test_that("export_doi5", {
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

test_that("export_doi6", {
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

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
