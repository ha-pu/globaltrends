# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
data <- filter(example_control, batch == 1 & location %in% c(gt.env$countries[1:2], "world"))
dbWriteTable(gt.env$globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o %in% 1:3 & location %in% c(gt.env$countries[1:2], "world"))
dbWriteTable(gt.env$globaltrends_db, "data_object", data, append = TRUE)
data <- filter(example_score, batch_c == 1 & batch_o %in% 1:3 & location %in% c(gt.env$countries[1:2], "world"))
dbWriteTable(gt.env$globaltrends_db, "data_score", data, append = TRUE)
data <- filter(example_doi, batch_c == 1 & batch_o %in% 1:3 & locations == "countries")
dbWriteTable(gt.env$globaltrends_db, "data_doi", data, append = TRUE)

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
  test_keyword(fun = export_object)
})

test_that("export_object6", {
  test_object(fun = export_object)
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
  test_keyword(fun = export_object_global)
})

test_that("export_object_global6", {
  test_object(fun = export_object_global)
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
  test_keyword(fun = export_score)
})

test_that("export_score6", {
  test_object(fun = export_score)
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
  test_keyword(fun = export_voi)
})

test_that("export_voi6", {
  test_object(fun = export_voi)
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
  expect_equal(nrow(out), 1200)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi2", {
  out <- export_doi(keyword = "manchester united")
  expect_equal(nrow(out), 120)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi3", {
  out <- export_doi(keyword = NULL)
  expect_equal(nrow(out), 1200)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi4", {
  out <- export_doi(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 240)
  expect_s3_class(out, "exp_doi")
})

test_that("export_doi5", {
  test_keyword(fun = export_doi)
})

test_that("export_doi6", {
  test_object(fun = export_doi)
  expect_equal(
    nrow(export_doi(object = 1:5)),
    1200
  )
})

test_that("export_doi7", {
  test_control(fun = export_doi)
  expect_equal(
    nrow(export_doi(control = 1:5)),
    1200
  )
})

test_that("export_doi9", {
  test_locations(fun = export_doi)
  expect_equal(
    nrow(export_doi(locations = c("countries", "us_states"))),
    1200
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
