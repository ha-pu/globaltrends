# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(purrr))

initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", data_score, append = TRUE)
dbWriteTable(globaltrends_db, "data_doi", data_doi, append = TRUE)

# plot score -------------------------------------------------------------------
test_that("plot_score1", {
  data <- export_score(keyword = "fc bayern", locations = countries)
  out1 <- plot_score(data_score = data, type = "obs")
  expect_s3_class(out1, "ggplot")
  
  data <- export_score(object = 1, locations = countries)
  expect_warning(
    out2 <- plot_score(data_score = data, type = "obs"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_score2", {
  data <- export_score(keyword = "fc bayern", locations = countries)
  out1 <- plot_score(data_score = data, type = "sad")
  expect_s3_class(out1, "ggplot")
  
  data <- export_score(object = 1, locations = countries)
  expect_warning(
    out2 <- plot_score(data_score = data, type = "sad"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_score3", {
  data <- export_score(keyword = "fc bayern", locations = countries)
  out1 <- plot_score(data_score = data, type = "trd")
  expect_s3_class(out1, "ggplot")
  
  data <- export_score(object = 1, locations = countries)
  expect_warning(
    out2 <- plot_score(data_score = data, type = "trd"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot score defaults ----------------------------------------------------------
test_that("plot_score_def", {
  data <- export_score(keyword = "fc bayern", locations = countries)
  out1 <- plot_score(data_score = data)
  out2 <- plot_score(data_score = data, type = "obs")
  out3 <- plot_score(data_score = data, type = "sad")
  out4 <- plot_score(data_score = data, type = "trd")
  
  expect_identical(out1, out2)
  expect_false(is.identical(out2, out3))
  expect_false(is.identical(out2, out4))
  expect_false(is.identical(out3, out4))
})

# plot doi ts gini -------------------------------------------------------------
test_that("plot_doi_ts1a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts2a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts3a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "gini", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot doi ts hhi --------------------------------------------------------------
test_that("plot_doi_ts1b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "hhi", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts2b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "hhi", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts3b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "hhi", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot doi ts entropy ----------------------------------------------------------
test_that("plot_doi_ts1c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "entropy", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts2c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "entropy", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts3c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  out1 <- plot_doi_ts(data, measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  expect_warning(
    out2 <- plot_doi_ts(data, measure = "entropy", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot doi ts defaults ---------------------------------------------------------
# plot doi box gini ------------------------------------------------------------
test_that("plot_doi_box1a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_doi_box(data, measure = "gini")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "gini"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_box2a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  out1 <- plot_doi_box(data, measure = "gini")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "gini"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_box3a", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  out1 <- plot_doi_box(data, measure = "gini")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "gini"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot doi box hhi -------------------------------------------------------------
test_that("plot_doi_box1b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_doi_box(data, measure = "hhi")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "hhi"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_box2b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  out1 <- plot_doi_box(data, measure = "hhi")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "hhi"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_box3b", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  out1 <- plot_doi_box(data, measure = "hhi")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "hhi"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot doi box entropy ---------------------------------------------------------
test_that("plot_doi_box1c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  out1 <- plot_doi_box(data, measure = "entropy")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "obs", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "entropy"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_box2c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  out1 <- plot_doi_box(data, measure = "entropy")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "sad", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "entropy"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_box3c", {
  keywords <- unique(data_doi$keyword)[1:9]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  out1 <- plot_doi_box(data, measure = "entropy")
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_doi$keyword)[1:10]
  data <- map_dfr(keywords, export_doi, type = "trd", locations = "countries")
  expect_warning(
    out2 <- plot_doi_box(data, measure = "entropy"),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot doi box defaults --------------------------------------------------------
# plot voi ts ------------------------------------------------------------------
test_that("plot_voi_ts1", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_ts(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_voi_ts(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_voi_ts2", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_ts(data, type = "sad", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_voi_ts(data, type = "sad", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_voi_ts3", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_ts(data, type = "trd", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_voi_ts(data, type = "trd", smooth = TRUE),
    "The plot function is limited to 9 keywords in a grid\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot voi ts defaults ---------------------------------------------------------
test_that("plot_voi_ts_def", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_ts(data, smooth = TRUE)
  out2 <- plot_voi_ts(data, type = "obs", smooth = TRUE)
  out3 <- plot_voi_ts(data, type = "sad", smooth = TRUE)
  out4 <- plot_voi_ts(data, type = "trd", smooth = TRUE)
  
  expect_identical(out1, out2)
  expect_false(is.identical(out2, out3))
  expect_false(is.identical(out2, out4))
  expect_false(is.identical(out3, out4))
})

# plot voi box -----------------------------------------------------------------
test_that("plot_voi_box1", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_box(data, type = "obs", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_voi_box(data, type = "obs", smooth = TRUE),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_voi_box2", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_box(data, type = "sad", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_voi_box(data, type = "sad", smooth = TRUE),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_voi_box3", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_box(data, type = "trd", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  keywords <- unique(data_score$keyword)[1:10]
  data <- map_dfr(keywords, export_voi)
  expect_warning(
    out2 <- plot_voi_box(data, type = "trd", smooth = TRUE),
    "The plot function is limited to 9 keywords in a boxplot\\.\nYou use 10 keywords\\.\nOnly the first 9 keywords are used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot voi box defaults --------------------------------------------------------
test_that("plot_voi_box_def", {
  keywords <- unique(data_score$keyword)[1:9]
  data <- map_dfr(keywords, export_voi)
  out1 <- plot_voi_box(data, smooth = TRUE)
  out2 <- plot_voi_box(data, type = "obs", smooth = TRUE)
  out3 <- plot_voi_box(data, type = "sad", smooth = TRUE)
  out4 <- plot_voi_box(data, type = "trd", smooth = TRUE)
  
  expect_identical(out1, out2)
  expect_false(is.identical(out2, out3))
  expect_false(is.identical(out2, out4))
  expect_false(is.identical(out3, out4))
})

# plot voi doi gini ------------------------------------------------------------
test_that("plot_voi_doi1a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "obs"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "obs"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts2a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "sad"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "sad"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts3a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "trd"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "trd"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "gini", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot voi doi hhi -------------------------------------------------------------
test_that("plot_voi_doi1a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "obs"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "obs"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts2a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "sad"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "sad"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts3a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "trd"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "trd"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "hhi", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot voi doi entropy ---------------------------------------------------------
test_that("plot_voi_doi1a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "obs"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "obs"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts2a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "sad"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "sad"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

test_that("plot_doi_ts3a", {
  data1 <- export_voi(keyword = "manchester united")
  data2 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "trd"
  )
  out1 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE)
  expect_s3_class(out1, "ggplot")
  
  data1 <- export_voi(object = 1)
  data2 <- export_doi(
    object = 1,
    locations = "countries",
    type = "trd"
  )
  expect_warning(
    out2 <- plot_voi_doi(data1, data2, measure = "entropy", smooth = TRUE),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")
  
  expect_identical(out1, out2)
})

# plot voi doi defaults --------------------------------------------------------
# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
