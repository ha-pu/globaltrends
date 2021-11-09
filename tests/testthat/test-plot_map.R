# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", example_score, append = TRUE)

# plot_map.exp_score ------------------------------------------------------------
test_that("plot_map.exp_score1", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_score)
  out1 <- plot_map(data, type = "obs")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score)
  expect_warning(
    out2 <- plot_map(data, type = "obs"),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_map.exp_score2", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_score)
  out1 <- plot_map(data, type = "sad")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score)
  expect_warning(
    out2 <- plot_map(data, type = "sad"),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_map.exp_score3", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_score)
  out1 <- plot_map(data, type = "trd")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score)
  expect_warning(
    out2 <- plot_map(data, type = "trd"),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_map.exp_score4", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_score)
  out1 <- plot_map(data)
  out2 <- plot_score_map(data)
  expect_identical(out1$labels, out2$labels)
})

# plot_map.exp_score defaults ---------------------------------------------------
test_that("plot_map.exp_score5", {
  keywords <- unique(example_score$keyword)[1]
  data <- map_dfr(keywords, export_score)
  out1 <- plot_map(data)
  out2 <- plot_map(data, type = "obs")
  out3 <- plot_map(data, type = "sad")
  out4 <- plot_map(data, type = "trd")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# plot_map.exp_score signals ----------------------------------------------------
test_that("plot_map.exp_scoreS1", {
  data <- export_score(keyword = "fc barcelona")
  expect_error(
    plot_map(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_map(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_map(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_map(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_map(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

# plot_map.abnorm_score ---------------------------------------------------------
test_that("plot_map.abnorm_score1", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "US")
  data <- get_abnorm_hist(data)
  out1 <- plot_map(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(example_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score, location = "US")
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_map(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_map.abnorm_score2", {
  data <- export_score(keyword = unique(example_score$keyword)[[1]], location = "US")
  data <- get_abnorm_hist(data)
  out1 <- plot_map(data)
  out2 <- plot_abnorm_score_map(data)
  expect_identical(out1$labels, out2$labels)
})

# plot_map methods --------------------------------------------------------------
test_that("plot_map.methods", {
  data <- export_control(control = 1)
  expect_error(
    plot_map(data),
    "no applicable method"
  )
  data <- export_object(object = 1)
  expect_error(
    plot_map(data),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
