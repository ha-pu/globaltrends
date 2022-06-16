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

# plot_box.exp_score ------------------------------------------------------------
test_that("plot_box.exp_score1", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_box(data, type = "obs")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_box(data, type = "obs"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_score2", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_box(data, type = "sad")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_box(data, type = "sad"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_score3", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_box(data, type = "trd")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_box(data, type = "trd"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_score4", {
  data <- export_score(keyword = example_score$keyword[[1]]) %>%
    filter(location == "CN")
  out1 <- plot_box(data, type = "obs")
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = example_score$keyword[[1]])
  expect_warning(
    out2 <- plot_box(data, type = "obs"),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_score5", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_box(data)
  out2 <- plot_score_box(data)
  expect_identical(out1$labels, out2$labels)
})

# plot_box.abnorm_score ---------------------------------------------------------
test_that("plot_box.abnorm_score1", {
  data <- export_score(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  test_ci(fun = plot_box, data = data)
})

test_that("plot_box.abnorm_score2", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "US")
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score, location = "US")
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.abnorm_score3", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "CN")
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = unique(example_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.abnorm_score4", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "CN")
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  out2 <- plot_abnorm_score_box(data)

  expect_identical(out1$labels, out2$labels)
})

# plot_box.exp_voi --------------------------------------------------------------
test_that("plot_box.exp_voi1", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_box(data, type = "obs")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_box(data, type = "obs"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_voi2", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_box(data, type = "sad")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_box(data, type = "sad"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_voi3", {
  keywords <- unique(example_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_box(data, type = "trd")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_box(data, type = "trd"),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.exp_voi4", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_box(data)
  out2 <- plot_voi_box(data)
  expect_identical(out1$labels, out2$labels)
})

# plot_box.abnorm_voi -----------------------------------------------------------
test_that("plot_box.abnorm_voi1", {
  data <- export_voi(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  test_ci(fun = plot_box, data = data)
})

test_that("plot_box.abnorm_voi2", {
  data <- export_voi(keyword = unique(example_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_voi)
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.abnorm_voi3", {
  data <- export_voi(keyword = unique(example_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  out2 <- plot_abnorm_voi_box(data)

  expect_identical(out1$labels, out2$labels)
})

# plot_box.exp_doi gini ---------------------------------------------------------
test_that("plot_box.exp_doi1a", {
  keywords <- unique(example_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_box(data, type = "obs", measure = "gini")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_box(data, type = "obs", measure = "gini"),
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

# plot_box.abnorm_doi -----------------------------------------------------------
test_that("plot_box.abnorm_doi1", {
  data <- export_doi(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  test_ci(fun = plot_box, data = data)
})

test_that("plot_box.abnorm_doi2", {
  data <- export_doi(keyword = unique(example_doi$keyword)[[1]], locations = "countries")
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_doi$keyword)[1:2]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_box(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_box.abnorm_doi3", {
  data <- export_doi(keyword = unique(example_doi$keyword)[[1]], locations = "countries")
  data <- get_abnorm_hist(data)
  out1 <- plot_box(data)
  out2 <- plot_abnorm_doi_box(data)

  expect_identical(out1$labels, out2$labels)
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
