# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

source("../test_functions.r")

initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(gt.env$globaltrends_db, "data_score", example_score, append = TRUE)
dbWriteTable(gt.env$globaltrends_db, "data_doi", example_doi, append = TRUE)

# plot voi doi gini ------------------------------------------------------------
test_that("plot_voi_doi-gini1", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "obs", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "obs", measure = "gini", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_voi_doi signals ---------------------------------------------------------
test_that("plot_voi_doi-sig1", {
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

test_that("plot_voi_doi-sig2", {
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

test_that("plot_voi_doi-sig5", {
  data1 <- export_doi(keyword = "fc barcelona")
  data2 <- export_voi(keyword = "fc barcelona")
  test_locations(fun = plot_voi_doi, incl = TRUE, data_doi = data1, data_voi = data2)
})

test_that("plot_voi_doi-sig7", {
  data1 <- export_voi()
  data2 <- export_doi()
  expect_warning(
    plot_voi_doi(data_voi = data1, data_doi = data2),
    "The plot function is limited to 1 keyword.\nYou use 14 keywords.\nOnly the first keyword is used."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
