# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

source("../test_functions.r")

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
  out <- export_control()
  expect_equal(nrow(out), 1200)
})

test_that("export_control2", {
  out <- export_control(control = 1)
  expect_equal(nrow(out), 1200)
})

test_that("export_control3", {
  out <- export_control(control = NULL)
  expect_equal(nrow(out), 1200)
})

test_that("export_control4", {
  out <- export_control(control = list(1))
  expect_equal(nrow(out), 1200)
})

test_that("export_control5", {
  test_control(fun = export_control)
})

# export_control_global --------------------------------------------------------
test_that("export_control_global1", {
  out <- export_control_global()
  expect_equal(nrow(out), 600)
})

test_that("export_control_global2", {
  out <- export_control_global(control = 1)
  expect_equal(nrow(out), 600)
})

test_that("export_control_global3", {
  out <- export_control_global(control = NULL)
  expect_equal(nrow(out), 600)
})

test_that("export_control_global4", {
  out <- export_control_global(control = list(1))
  expect_equal(nrow(out), 600)
})

test_that("export_control_global5", {
  test_control(fun = export_control_global)
})

# export_object ----------------------------------------------------------------
test_that("export_object1", {
  out <- export_object()
  expect_equal(nrow(out), 3120)
})

test_that("export_object2", {
  out <- export_object(keyword = "manchester united")
  expect_equal(nrow(out), 240)
})

test_that("export_object3", {
  out <- export_object(keyword = NULL)
  expect_equal(nrow(out), 3120)
})

test_that("export_object4", {
  out <- export_object(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 480)
})

test_that("export_object5", {
  expect_error(
    export_object(keyword = 1),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_object(keyword = TRUE),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_object(keyword = sum),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("export_object6", {
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
  expect_equal(
    nrow(export_object(object = 1:5)),
    3120
  )
})

test_that("export_object7", {
  test_control(fun = export_object)
  
  expect_equal(
    nrow(export_object(control = 1:5)),
    3120
  )
})

# export_object_global ---------------------------------------------------------
test_that("export_object_global1", {
  out <- export_object_global()
  expect_equal(nrow(out), 1560)
})

test_that("export_object_global2", {
  out <- export_object_global(keyword = "manchester united")
  expect_equal(nrow(out), 120)
})

test_that("export_object_global3", {
  out <- export_object_global(keyword = NULL)
  expect_equal(nrow(out), 1560)
})

test_that("export_object_global4", {
  out <- export_object_global(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 240)
})

test_that("export_object_global5", {
  expect_error(
    export_object_global(keyword = 1),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_object_global(keyword = TRUE),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_object_global(keyword = sum),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("export_object_global6", {
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
  expect_equal(
    nrow(export_object_global(object = 1:5)),
    1560
  )
})

test_that("export_object_global7", {
  test_control(fun = export_object_global)
  
  expect_equal(
    nrow(export_object_global(control = 1:5)),
    1560
  )
})

# export_score -----------------------------------------------------------------
test_that("export_score1", {
  out <- export_score()
  expect_equal(nrow(out), 1920)
  expect_s3_class(out, "exp_score")
})

test_that("export_score2", {
  out <- export_score(keyword = "manchester united")
  expect_equal(nrow(out), 240)
  expect_s3_class(out, "exp_score")
})

test_that("export_score3", {
  out <- export_score(keyword = NULL)
  expect_equal(nrow(out), 1920)
  expect_s3_class(out, "exp_score")
})

test_that("export_score4", {
  out <- export_score(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 480)
  expect_s3_class(out, "exp_score")
})

test_that("export_score5", {
  expect_error(
    export_score(keyword = 1),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_score(keyword = TRUE),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_score(keyword = sum),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("export_score6", {
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
  expect_equal(
    nrow(export_score(object = 1:5)),
    1920
  )
})

test_that("export_score7", {
  test_control(fun = export_score)
  
  expect_equal(
    nrow(export_score(control = 1:5)),
    1920
  )
})

# export_voi -------------------------------------------------------------------
test_that("export_voi1", {
  out <- export_voi()
  expect_equal(nrow(out), 1200)
  expect_s3_class(out, "exp_voi")
})

test_that("export_voi2", {
  out <- export_voi(keyword = "manchester united")
  expect_equal(nrow(out), 120)
  expect_s3_class(out, "exp_voi")
})

test_that("export_voi3", {
  out <- export_voi(keyword = NULL)
  expect_equal(nrow(out), 1200)
  expect_s3_class(out, "exp_voi")
})

test_that("export_voi4", {
  out <- export_voi(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 240)
  expect_s3_class(out, "exp_voi")
})

test_that("export_voi5", {
  expect_error(
    export_voi(keyword = 1),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_voi(keyword = TRUE),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_voi(keyword = sum),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("export_voi6", {
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
  expect_equal(
    nrow(export_voi(object = 1:5)),
    1200
  )
})

test_that("export_voi7", {
  test_control(fun = export_voi)
  
  expect_equal(
    nrow(export_voi(control = 1:5)),
    1200
  )
})

# export_doi -------------------------------------------------------------------
test_that("export_doi1", {
  out <- export_doi()
  expect_equal(nrow(out), 3600)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi2", {
  out <- export_doi(keyword = "manchester united")
  expect_equal(nrow(out), 360)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi3", {
  out <- export_doi(keyword = NULL)
  expect_equal(nrow(out), 3600)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi4", {
  out <- export_doi(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 720)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi5", {
  expect_error(
    export_doi(keyword = 1),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    export_doi(keyword = TRUE),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    export_doi(keyword = sum),
    "Error: 'keyword' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("export_doi6", {
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
  expect_equal(
    nrow(export_doi(object = 1:5)),
    3600
  )
})

test_that("export_doi7", {
  test_control(fun = export_doi)
  
  expect_equal(
    nrow(export_doi(control = 1:5)),
    3600
  )
})

test_that("export_doi8", {
  test_type(fun = export_doi, incl = 2:5)
  
  expect_equal(
    nrow(export_doi(type = c("obs", "sad", "trd"))),
    3600
  )
})

test_that("export_doi9", {
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
  expect_equal(
    nrow(export_doi(locations = c("countries", "us_states"))),
    3600
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
