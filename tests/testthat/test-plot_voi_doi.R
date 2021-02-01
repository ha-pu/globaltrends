# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", example_score, append = TRUE)
dbWriteTable(globaltrends_db, "data_doi", example_doi, append = TRUE)

# plot voi doi gini ------------------------------------------------------------
test_that("plot_voi_doi1a", {
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

test_that("plot_voi_doi2a", {
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

test_that("plot_voi_doi3a", {
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
test_that("plot_voi_doi1b", {
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

test_that("plot_voi_doi2b", {
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

test_that("plot_voi_doi3b", {
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
test_that("plot_voi_doi1c", {
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

test_that("plot_voi_doi2c", {
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

test_that("plot_voi_doi3c", {
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
test_that("plot_voi_doi_def1", {
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

test_that("plot_voi_doi_def2", {
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

test_that("plot_voi_doi_def3", {
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

test_that("plot_voi_doi_def4", {
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

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
