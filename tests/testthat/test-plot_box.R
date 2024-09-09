# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbAppendTable(gt.env$globaltrends_db, "data_score", example_score)
dbAppendTable(gt.env$globaltrends_db, "data_doi", example_doi)

# plot_box.exp_score ------------------------------------------------------------
test_that("plot_box.exp_score1", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_score4", {
  data <- export_score(keyword = example_score$keyword[[1]]) %>%
    filter(location == "CN")
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = example_score$keyword[[1]])
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_box.exp_voi --------------------------------------------------------------
test_that("plot_box.exp_voi1", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_box.exp_doi gini ---------------------------------------------------------
test_that("plot_box.exp_doi1a", {
  keywords <- unique(example_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_box(data, measure = "gini")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_box(data, measure = "gini"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_box.exp_doi defaults -----------------------------------------------------
test_that("plot_box.exp_doiS3", {
  data <- export_doi(keyword = "fc barcelona")
  test_locations(fun = plot_box, incl = TRUE, data = data)
})

# plot_box methods --------------------------------------------------------------
test_that("plot_box.methods", {
  data <- export_control(control = 1)
  expect_error(
    plot_box(data),
    "no applicable method"
  )
  data <- export_object(object = 1)
  expect_error(
    plot_box(data),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
