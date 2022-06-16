# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(gt.env$globaltrends_db, "data_score", example_score, append = TRUE)
dbWriteTable(gt.env$globaltrends_db, "data_doi", example_doi, append = TRUE)

# plot_ts.exp_score ------------------------------------------------------------
test_that("plot_ts.exp_score1", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_score4", {
  data <- export_score(keyword = example_score$keyword[[1]]) %>%
    filter(location == "CN")
  out1 <- plot_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = example_score$keyword[[1]])
  expect_warning(
    out2 <- plot_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_score5", {
  data <- export_score(keyword = example_score$keyword[[1]]) %>%
    filter(location == "CN")
  out1 <- plot_ts(data)
  out2 <- plot_score_ts(data)

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.abnorm_score ---------------------------------------------------------
test_that("plot_ts.abnorm_score1", {
  data <- export_score(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  test_ci(fun = plot_ts, data = data)
})

test_that("plot_ts.abnorm_score2", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "US")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score, location = "US")
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.abnorm_score3", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "CN")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = unique(example_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.abnorm_score4", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "CN")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  out2 <- plot_abnorm_score_ts(data)

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_voi --------------------------------------------------------------
test_that("plot_ts.exp_voi1", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.abnorm_voi -----------------------------------------------------------
test_that("plot_ts.abnorm_voi1", {
  data <- export_voi(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  test_ci(fun = plot_ts, data = data)
})

test_that("plot_ts.abnorm_voi2", {
  data <- export_voi(keyword = unique(example_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_voi)
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.abnorm_voi3", {
  data <- export_voi(keyword = unique(example_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  out2 <- plot_abnorm_voi_ts(data)

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_doi gini ---------------------------------------------------------
test_that("plot_ts.exp_doi1a", {
  keywords <- unique(example_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "obs", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "obs", measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_doi signals ------------------------------------------------------
test_that("plot_ts.exp_doiS3", {
  data <- export_doi(keyword = "fc barcelona")
  test_locations(fun = plot_ts, incl = TRUE, data = data)
})

# plot_ts.abnorm_doi -----------------------------------------------------------
test_that("plot_ts.abnorm_doi1", {
  data <- export_doi(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  test_ci(fun = plot_ts, data = data)
})

test_that("plot_ts.abnorm_doi2", {
  data <- export_doi(keyword = unique(example_doi$keyword)[[1]], locations = "countries")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_doi$keyword)[1:2]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.abnorm_doi3", {
  data <- export_doi(keyword = unique(example_doi$keyword)[[1]], locations = "countries")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  out2 <- plot_abnorm_doi_ts(data)

  expect_identical(out1$labels, out2$labels)
})

# plot_ts methods --------------------------------------------------------------
test_that("plot_ts.methods", {
  data <- export_control(control = 1)
  expect_error(
    plot_ts(data),
    "no applicable method"
  )
  data <- export_object(object = 1)
  expect_error(
    plot_ts(data),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
