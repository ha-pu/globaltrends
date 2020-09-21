library(dplyr)
library(ggplot2)

# initialize and start ----
test_that("initialize", {
  setwd(tempdir())
  expect_message(
    initialize_db()
  )
  expect_message(
    start_db()
  )
  rm(
    tbl_control,
    tbl_doi,
    tbl_global,
    tbl_mapping,
    tbl_object,
    tbl_score,
    keyword_synonyms,
    keywords_control,
    keywords_object,
    time_control,
    time_object,
    envir = .GlobalEnv
  )
})

# add keywords ----
test_that("keywords_control", {
  expect_message(
    new_batch <- add_control_keyword(
      keyword = c(
        "gmail",
        "map",
        "translate",
        "wikipedia",
        "youtube"
      ),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch == new_batch & type == "control")
  out_time <- filter(.tbl_time, batch == new_batch & type == "control")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 5)
  expect_equal(nrow(out_time), 1)
})

test_that("keywords_object", {
  expect_message(
    new_batch <- add_object_keyword(
      keyword = c(
        "fc barcelona",
        "fc bayern",
        "liverpool fc",
        "manchester united",
        "real madrid"
      ),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch == new_batch & type == "object")
  out_time <- filter(.tbl_time, batch == new_batch & type == "object")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 5)
  expect_equal(nrow(out_time), 1)
})

# run downloads ----
test_that("control_download", {
  expect_message(
    download_control(control = 1, locations = countries[1:3])
  )
  out <- filter(.tbl_control, batch == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

test_that("object_download", {
  expect_message(
    download_object(object = 1, locations = countries[1:3])
  )
  out <- filter(.tbl_object, batch == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

test_that("mapping_download", {
  expect_message(
    download_mapping(
      control = 1,
      object = 1,
      locations = countries[1:3]
    )
  )
  out <- filter(.tbl_mapping, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_equal(nrow(out), 720)
})

test_that("global_download", {
  expect_message(
    download_global(object = 1)
  )
  out <- filter(.tbl_global, batch == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

# compute data ----
test_that("compute_scoring", {
  expect_message(
    compute_score(
      control = 1,
      object = 1,
      locations = countries[1:3]
    )
  )
  out <- filter(.tbl_score, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

test_that("compute_doi", {
  expect_message(
    compute_doi(
      control = 1,
      object = 1,
      locations = "countries"
    )
  )
  out <- filter(.tbl_doi, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

# export data ----
test_that("export_control", {
  out <- export_control(control = 1)
  expect_equal(nrow(out), 1800)
})

test_that("export_object", {
  out <- export_object(keyword = "manchester united")
  expect_equal(nrow(out), 360)
})

test_that("export_mapping", {
  out <- export_mapping(control = 1, object = 1)
  expect_equal(nrow(out), 720)
})

test_that("export_global", {
  out <- export_global(type = "sad")
  expect_equal(nrow(out), 600)
})

test_that("export_score", {
  out <- export_score(keyword = "real madrid")
  expect_equal(nrow(out), 360)
})

test_that("export_doi", {
  out <- export_doi(
    control = 1,
    object = 1,
    type = "trd",
    locations = "countries"
  )
  expect_equal(nrow(out), 600)
})

# plot data ----
test_that("plot_score", {
  out <- export_score(keyword = "fc bayern") %>%
    filter(location %in% countries) %>%
    plot_score(type = "sad")
  expect_s3_class(out, "ggplot")
})

test_that("plot_ts", {
  out <- export_doi(type = "obs", locations = "countries") %>%
    plot_ts(grid = TRUE, smooth = TRUE)
  expect_s3_class(out, "ggplot")
})

test_that("plot_box", {
  out <- export_doi(type = "sad", locations = "countries") %>%
    plot_box()
  expect_s3_class(out, "ggplot")
})

test_that("plot_trend", {
  data1 <- export_doi(keyword = "liverpool fc", locations = "countries")
  data2 <- export_global(keyword = "liverpool fc")
  out <- plot_trend(
    data_doi = data1,
    data_global = data2,
    type = "obs",
    measure = "gini",
    smooth = TRUE
  )
  expect_s3_class(out, "ggplot")
})

# remove data ----
test_that("remove_control", {
  expect_message(
    remove_data(table = "batch_keywords", control = 1)
  )
  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "control")
  out_time <- filter(.tbl_time, batch == 1 & type == "control")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 0)
  expect_equal(nrow(out_time), 0)
})

test_that("remove_object", {
  expect_message(
    remove_data(table = "batch_keywords", object = 1)
  )
  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "object")
  out_time <- filter(.tbl_time, batch == 1 & type == "object")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 0)
  expect_equal(nrow(out_time), 0)
})

# disconnect ----
test_that("disconnect", {
  expect_message(disconnect_db())
})
