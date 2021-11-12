# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

source("../test_functions.r")

initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", example_score, append = TRUE)
dbWriteTable(globaltrends_db, "data_doi", example_doi, append = TRUE)

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

test_that("plot_voi_doi-gini2", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "sad", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "sad", measure = "gini", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi_doi-gini3", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "trd", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "trd", measure = "gini", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot voi doi hhi -------------------------------------------------------------
test_that("plot_voi_doi-hhi1", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "obs", measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "obs", measure = "hhi", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi_doi-hhi2", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "sad", measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "sad", measure = "hhi", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi_doi-hhi3", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "trd", measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "trd", measure = "hhi", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot voi doi entropy ---------------------------------------------------------
test_that("plot_voi_doi-entropy1", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "obs", measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(object = 1, locations = "countries")
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "obs", measure = "entropy", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi_doi-entropy2", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "sad", measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "sad", measure = "entropy", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi_doi-entropy3", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, type = "trd", measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, type = "trd", measure = "entropy", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot voi doi defaults --------------------------------------------------------
test_that("plot_voi_doi-def1", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE)
  out2 <- plot_voi_doi(data1, data2, type = "obs", measure = "gini", smooth = TRUE)
  out3 <- plot_voi_doi(data1, data2, type = "sad", measure = "gini", smooth = TRUE)
  out4 <- plot_voi_doi(data1, data2, type = "trd", measure = "gini", smooth = TRUE)

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("plot_voi_doi-def2", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE)
  out2 <- plot_voi_doi(data1, data2, type = "obs", measure = "hhi", smooth = TRUE)
  out3 <- plot_voi_doi(data1, data2, type = "sad", measure = "hhi", smooth = TRUE)
  out4 <- plot_voi_doi(data1, data2, type = "trd", measure = "hhi", smooth = TRUE)

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("plot_voi_doi-def3", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE)
  out2 <- plot_voi_doi(data1, data2, type = "obs", measure = "entropy", smooth = TRUE)
  out3 <- plot_voi_doi(data1, data2, type = "sad", measure = "entropy", smooth = TRUE)
  out4 <- plot_voi_doi(data1, data2, type = "trd", measure = "entropy", smooth = TRUE)

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("plot_voi_doi-def4", {
  data1 <- export_voi(keyword = "fc barcelona")
  data2 <- export_doi(keyword = "fc barcelona", locations = "countries")
  out1 <- plot_voi_doi(data1, data2, smooth = TRUE)
  out2 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE)
  out3 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE)
  out4 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE)

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
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

test_that("plot_voi_doi-sig3", {
  test_type(fun = plot_voi_doi, data_doi = export_doi(keyword = "fc barcelona"), data_voi = export_voi(keyword = "fc barcelona"))
})

test_that("plot_voi_doi-sig4", {
  test_measure(fun = plot_voi_doi, data_doi = export_doi(keyword = "fc barcelona"), data_voi = export_voi(keyword = "fc barcelona"))
})

test_that("plot_voi_doi-sig5", {
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

test_that("plot_voi_doi-sig6", {
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
