# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", data_score, append = TRUE)
dbWriteTable(globaltrends_db, "data_doi", data_doi, append = TRUE)

# plot_ts.exp_score ------------------------------------------------------------
test_that("plot_ts.exp_score1", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_score2", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_ts(data, type = "sad", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_ts(data, type = "sad", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_score3", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  out1 <- plot_ts(data, type = "trd", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "US")
  expect_warning(
    out2 <- plot_ts(data, type = "trd", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_score4", {
  data <- export_score(keyword = data_score$keyword[[1]]) %>%
    filter(location == "CN")
  out1 <- plot_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = data_score$keyword[[1]])
  expect_warning(
    out2 <- plot_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_score defaults ---------------------------------------------------
test_that("plot_ts.exp_score5", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_score) %>%
    filter(location == "CN")
  out1 <- plot_ts(data, smooth = TRUE)
  out2 <- plot_ts(data, type = "obs", smooth = TRUE)
  out3 <- plot_ts(data, type = "sad", smooth = TRUE)
  out4 <- plot_ts(data, type = "trd", smooth = TRUE)

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# plot_ts.exp_score signals ----------------------------------------------------
test_that("plot_ts.exp_scoreS1", {
  data <- export_score(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_ts(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_ts.exp_scoreS2", {
  data <- export_score(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, smooth = 1),
    "'smooth' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, smooth = "A"),
    "'smooth' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    plot_ts(data, smooth = sum),
    "'smooth' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, smooth = c(TRUE, TRUE)),
    "'smooth' must be object of length 1.\nYou provided an object of length 2."
  )
})

# plot_ts.abnorm_score ---------------------------------------------------------
test_that("plot_ts.abnorm_score1", {
  data <- export_score(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  expect_error(
    plot_ts(data, ci = "A"),
    "'ci' must be object of type double.\nYou provided an object of type character."
  )
  expect_error(
    plot_ts(data, ci = TRUE),
    "'ci' must be object of type double.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, ci = sum),
    "'ci' must be object of type double.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, ci = 1:3 / 10),
    "'ci' must be object of length 1.\nYou provided an object of length 3."
  )
  expect_error(
    plot_ts(data, ci = 0),
    "'ci' must be greater than 0 and less than 1.\nYou provided 0."
  )
  expect_error(
    plot_ts(data, ci = 1),
    "'ci' must be greater than 0 and less than 1.\nYou provided 1."
  )
})

test_that("plot_ts.abnorm_score2", {
  data <- export_score(keyword = unique(data_score$keyword)[[1]], location = "US")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:2]
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
  data <- export_score(keyword = unique(data_score$keyword)[[1]], location = "CN")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  data <- export_score(keyword = unique(data_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 location\\.\nYou use 3 locations\\.\nOnly 'CN' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_voi --------------------------------------------------------------
test_that("plot_ts.exp_voi1", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_voi2", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_ts(data, type = "sad", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_ts(data, type = "sad", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_voi3", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_ts(data, type = "trd", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_ts(data, type = "trd", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_voi defaults -----------------------------------------------------
test_that("plot_ts.exp_voi4", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_ts(data, smooth = TRUE)
  out2 <- plot_ts(data, type = "obs", smooth = TRUE)
  out3 <- plot_ts(data, type = "sad", smooth = TRUE)
  out4 <- plot_ts(data, type = "trd", smooth = TRUE)

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# plot_ts.exp_voi signals ------------------------------------------------------
test_that("plot_ts.exp_voiS1", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_ts(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_ts.exp_voiS2", {
  data <- export_voi(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, smooth = 1),
    "'smooth' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, smooth = "A"),
    "'smooth' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    plot_ts(data, smooth = sum),
    "'smooth' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, smooth = c(TRUE, TRUE)),
    "'smooth' must be object of length 1.\nYou provided an object of length 2."
  )
})

# plot_ts.abnorm_voi -----------------------------------------------------------
test_that("plot_ts.abnorm_voi1", {
  data <- export_voi(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  expect_error(
    plot_ts(data, ci = "A"),
    "'ci' must be object of type double.\nYou provided an object of type character."
  )
  expect_error(
    plot_ts(data, ci = TRUE),
    "'ci' must be object of type double.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, ci = sum),
    "'ci' must be object of type double.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, ci = 1:3 / 10),
    "'ci' must be object of length 1.\nYou provided an object of length 3."
  )
  expect_error(
    plot_ts(data, ci = 0),
    "'ci' must be greater than 0 and less than 1.\nYou provided 0."
  )
  expect_error(
    plot_ts(data, ci = 1),
    "'ci' must be greater than 0 and less than 1.\nYou provided 1."
  )
})

test_that("plot_ts.abnorm_voi2", {
  data <- export_voi(keyword = unique(data_score$keyword)[[1]])
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_score$keyword)[1:2]
  data <- map_dfr(keywords, export_voi)
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_doi gini ---------------------------------------------------------
test_that("plot_ts.exp_doi1a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "obs", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "obs", measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_doi2a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "sad", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "sad", measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_doi3a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "trd", measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "trd", measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_doi hhi ----------------------------------------------------------
test_that("plot_ts.exp_doi1b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "obs", measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "obs", measure = "hhi", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_doi2b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "sad", measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "sad", measure = "hhi", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_doi3b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "trd", measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "trd", measure = "hhi", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_doi entropy ------------------------------------------------------
test_that("plot_ts.exp_doi1c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "obs", measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "obs", measure = "entropy", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts.exp_doi2c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "sad", measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "sad", measure = "entropy", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_ts3c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data, type = "trd", measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  expect_warning(
    out2 <- plot_ts(data, type = "trd", measure = "entropy", smooth = TRUE),
    "The plot function is limited to 9 keywords\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot_ts.exp_doi defaults -----------------------------------------------------
test_that("plot_ts.exp_doi4a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data = data, measure = "gini")
  out2 <- plot_ts(data = data, measure = "gini", type = "obs")
  out3 <- plot_ts(data = data, measure = "gini", type = "sad")
  out4 <- plot_ts(data = data, measure = "gini", type = "trd")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("plot_ts.exp_doi4b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data = data, measure = "hhi")
  out2 <- plot_ts(data = data, measure = "hhi", type = "obs")
  out3 <- plot_ts(data = data, measure = "hhi", type = "sad")
  out4 <- plot_ts(data = data, measure = "hhi", type = "trd")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("plot_ts.exp_doi4c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  out1 <- plot_ts(data = data, measure = "entropy")
  out2 <- plot_ts(data = data, measure = "entropy", type = "obs")
  out3 <- plot_ts(data = data, measure = "entropy", type = "sad")
  out4 <- plot_ts(data = data, measure = "entropy", type = "trd")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("plot_ts.exp_doi4d", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_ts(data = data)
  out2 <- plot_ts(data = data, measure = "gini")
  out3 <- plot_ts(data = data, measure = "hhi")
  out4 <- plot_ts(data = data, measure = "entropy")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# plot_ts.exp_doi signals ------------------------------------------------------
test_that("plot_ts.exp_doiS1", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    plot_ts(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_ts.exp_doiS2", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, measure = 1),
    "'measure' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, measure = "A"),
    "'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou provided A."
  )
  expect_error(
    plot_ts(data, measure = TRUE),
    "'measure' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, measure = sum),
    "'measure' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, measure = c("gini", "hhi", "entropy")),
    "'measure' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("plot_ts.exp_doiS3", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, locations = letters[1:5]),
    "'locations' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("plot_ts.exp_doiS4", {
  data <- export_doi(keyword = "fc barcelona")
  expect_error(
    plot_ts(data, smooth = 1),
    "'smooth' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    plot_ts(data, smooth = "A"),
    "'smooth' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    plot_ts(data, smooth = sum),
    "'smooth' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, smooth = c(TRUE, TRUE)),
    "'smooth' must be object of length 1.\nYou provided an object of length 2."
  )
})

# plot_ts.abnorm_doi -----------------------------------------------------------
test_that("plot_ts.abnorm_doi1", {
  data <- export_doi(keyword = "fc barcelona")
  data <- get_abnorm_hist(data)
  expect_error(
    plot_ts(data, ci = "A"),
    "'ci' must be object of type double.\nYou provided an object of type character."
  )
  expect_error(
    plot_ts(data, ci = TRUE),
    "'ci' must be object of type double.\nYou provided an object of type logical."
  )
  expect_error(
    plot_ts(data, ci = sum),
    "'ci' must be object of type double.\nYou provided an object of type builtin."
  )
  expect_error(
    plot_ts(data, ci = 1:3 / 10),
    "'ci' must be object of length 1.\nYou provided an object of length 3."
  )
  expect_error(
    plot_ts(data, ci = 0),
    "'ci' must be greater than 0 and less than 1.\nYou provided 0."
  )
  expect_error(
    plot_ts(data, ci = 1),
    "'ci' must be greater than 0 and less than 1.\nYou provided 1."
  )
})

test_that("plot_ts.abnorm_doi2", {
  data <- export_doi(keyword = unique(data_doi$keyword)[[1]], locations = "countries")
  data <- get_abnorm_hist(data)
  out1 <- plot_ts(data)
  expect_s3_class(out1, "ggplot")

  keywords <- unique(data_doi$keyword)[1:2]
  data <- map_dfr(keywords, export_doi, locations = "countries")
  data <- get_abnorm_hist(data)
  expect_warning(
    out2 <- plot_ts(data),
    "The plot function is limited to 1 keyword\\.\nYou use 2 keywords\\.\nOnly 'amazon' is used\\."
  )
  expect_s3_class(out2, "ggplot")

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
