# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", data_score, append = TRUE)

# plot_bar.exp_score ------------------------------------------------------------
test_that("plot_bar.exp_score1", {
  keywords <- unique(data_score$keyword)[1]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_bar(data, type = "obs")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_bar(data, type = "obs"),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_bar.exp_score2", {
  keywords <- unique(data_score$keyword)[1]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_bar(data, type = "sad")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_bar(data, type = "sad"),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_bar.exp_score3", {
  keywords <- unique(data_score$keyword)[1]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_bar(data, type = "trd")
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_bar(data, type = "trd"),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_bar.exp_score defaults ---------------------------------------------------
test_that("plot_bar.exp_score4", {
  keywords <- unique(data_score$keyword)[1]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "CN")
  out1 <- plot_bar(data)
  out2 <- plot_bar(data, type = "obs")
  out3 <- plot_bar(data, type = "sad")
  out4 <- plot_bar(data, type = "trd")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# plot_bar.exp_score signals ----------------------------------------------------
test_that("plot_bar.exp_scoreS1", {
  data <- export_score(keyword = "fc barcelona")
  expect_error(
    plot_bar(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_bar(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_bar(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_bar(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_bar(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

# plot_bar.abnorm_score ---------------------------------------------------------
test_that("plot_bar.abnorm_score1", {
  data <- export_score(keyword = unique(data_score$keyword)[[1]], location = "US")
  data <- compute_abnorm(data)
  out1 <- plot_bar(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:2]
  data <- map_dfr(keywords, export_score, location = "US")
  data <- compute_abnorm(data)
  expect_warning(
    out2 <- plot_bar(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_bar methods --------------------------------------------------------------
test_that("plot_bar.methods", {
  data <- export_control(control = 1)
  expect_error(
    plot_bar(data),
    "no applicable method"
  )
  data <- export_object(object = 1)
  expect_error(
    plot_bar(data),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
