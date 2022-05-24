# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))
suppressWarnings(library(lubridate))

initialize_db()
start_db()

# compute_score object = 12 ----------------------------------------------------
remove_data("data_control", control = 1)
remove_data("data_object", object = 1)
data <- filter(example_control, batch == 1)
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o == 1 & year(as_date(date)) == 2019)
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)
.keywords_object <- data %>%
  select(
    batch = batch_o,
    keyword
  ) %>%
  unique()
assign(".keywords_object", .keywords_object, envir = as.environment("package:globaltrends"))

test_that("score1", {
  expect_warning(
    compute_score(control = 1, object = 1),
    "You provided object data for less than 24 months.\nNo time series adjustments possible."
  )
})

test_that("score2", {
  expect_warning(
    compute_voi(control = 1, object = 1),
    "You provided object data for less than 24 months.\nNo time series adjustments possible."
  )
})

# compute_score control = 12 ----------------------------------------------------
remove_data("data_control", control = 1)
remove_data("data_object", object = 1)
data <- filter(example_control, batch == 1 & year(as_date(date)) == 2019)
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o == 1)
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)

test_that("score3", {
  expect_warning(
    compute_score(control = 1, object = 1),
    "You provided control data for less than 24 months.\nNo time series adjustments possible."
  )
})

test_that("score4", {
  expect_warning(
    compute_voi(control = 1, object = 1),
    "You provided control data for less than 24 months.\nNo time series adjustments possible."
  )
})

# compute_score control = 12, object = 12 --------------------------------------
remove_data("data_control", control = 1)
remove_data("data_object", object = 1)
data <- filter(example_control, batch == 1 & year(as_date(date)) == 2019)
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o == 1 & year(as_date(date)) == 2019)
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)

test_that("score5", {
  expect_warning(
    compute_score(control = 1, object = 1),
    "You provided control and object data for less than 24 months.\nNo time series adjustments possible."
  )
})

test_that("score6", {
  expect_warning(
    compute_voi(control = 1, object = 1),
    "You provided control and object data for less than 24 months.\nNo time series adjustments possible."
  )
})

# compute_doi ------------------------------------------------------------------
test_that("doi1", {
  expect_message(
    compute_doi(object = 1, control = 1),
    "Successfully computed DOI | control: 1 | object: 1 [1/1]"
  )
})

# plot_score -------------------------------------------------------------------
data <- export_score(keyword = "fc barcelona", control = 1, location = "US")

test_that("plot_score1", {
  expect_s3_class(
    plot_bar(data, type = "obs"),
    "ggplot"
  )
  expect_s3_class(
    plot_ts(data, type = "obs"),
    "ggplot"
  )
  expect_s3_class(
    plot_box(data, type = "obs"),
    "ggplot"
  )
})

test_that("plot_score2", {
  expect_warning(
    plot_bar(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_ts(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_box(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

test_that("plot_score3", {
  expect_warning(
    plot_bar(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_ts(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_box(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

# plot_voi ---------------------------------------------------------------------
data <- export_voi(keyword = "fc barcelona", control = 1)

test_that("plot_voi1", {
  expect_s3_class(
    plot_box(data, type = "obs"),
    "ggplot"
  )
  expect_s3_class(
    plot_ts(data, type = "obs"),
    "ggplot"
  )
})

test_that("plot_voi2", {
  expect_warning(
    plot_box(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_ts(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

test_that("plot_voi3", {
  expect_warning(
    plot_box(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_ts(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

# plot_doi ---------------------------------------------------------------------
data <- export_doi(keyword = "fc barcelona", control = 1)

test_that("plot_doi1", {
  expect_s3_class(
    plot_box(data, type = "obs"),
    "ggplot"
  )
  expect_s3_class(
    plot_ts(data, type = "obs"),
    "ggplot"
  )
})

test_that("plot_doi2", {
  expect_warning(
    plot_box(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_ts(data, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

test_that("plot_doi3", {
  expect_warning(
    plot_box(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
  expect_warning(
    plot_ts(data, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

# plot_voi_doi -----------------------------------------------------------------
data1 <- export_voi(keyword = "fc barcelona", control = 1)
data2 <- export_doi(keyword = "fc barcelona", control = 1)

test_that("plot_voi_doi1", {
  expect_s3_class(
    plot_voi_doi(data_voi = data1, data_doi = data2, type = "obs"),
    "ggplot"
  )
})

test_that("plot_voi_doi2", {
  expect_warning(
    plot_voi_doi(data_voi = data1, data_doi = data2, type = "sad"),
    "Plot cannot be created.\nThere is no non-missing data for score_sad in data_voi.\nThere is no non-missing data for score_sad in data_doi.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

test_that("plot_voi_doi3", {
  expect_warning(
    plot_voi_doi(data_voi = data1, data_doi = data2, type = "trd"),
    "Plot cannot be created.\nThere is no non-missing data for score_trd in data_voi.\nThere is no non-missing data for score_trd in data_doi.\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
