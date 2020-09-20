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
})

# add keywords ----
test_that("keywords_control", {
  expect_message(
    new_batch <- add_control_keyword(keyword = c(
      "gmail",
      "map",
      "translate",
      "wikipedia",
      "youtube"
    ))
  )
  out_keywords <- filter(batch_keywords, batch == new_batch & type == "control")
  out_time <- filter(batch_time, batch == new_batch & type == "control")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 5)
  expect_equal(nrow(out_time), 1)
})

test_that("keywords_object", {
  expect_message(
    new_batch <- add_object_keyword(keyword = c(
      "fc barcelona",
      "fc bayern",
      "liverpool fc",
      "manchester united",
      "real madrid"
    ))
  )
  out_keywords <- filter(batch_keywords, batch == new_batch & type == "object")
  out_time <- filter(batch_time, batch == new_batch & type == "object")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 5)
  expect_equal(nrow(out_time), 1)
})

# run downloads ----
test_that("control_download", {
  expect_message(
    download_control(control = 1, locations = countries[1:5])
  )
  out <- filter(data_control, batch == 1)
  out <- collect(out)
  expect_named(out)
})

test_that("object_download", {
  expect_message(
    download_object(object = 1, locations = countries[1:5])
  )
  out <- filter(data_object, batch == 1)
  out <- collect(out)
  expect_named(out)
})

test_that("mapping_download", {
  expect_message(
    download_mapping(
      control = 1,
      object = 1,
      locations = countries[1:5]
    )
  )
  out <- filter(data_mapping, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_named(out)
})

test_that("global_download", {
  expect_message(
    download_global(object = 1)
  )
  out <- filter(data_global, batch == 1)
  out <- collect(out)
  expect_named(out)
})

# compute data ----
test_that("compute_scoring", {
  expect_message(
    compute_score(
      control = 1,
      object = 1,
      locations = countries[1:5]
    )
  )
  out <- filter(data_score, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_named(out)
})

test_that("compute_doi", {
  expect_message(
    compute_doi(
      control = 1,
      object = 1,
      locations = "countries"
    )
  )
  out <- filter(data_doi, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_named(out)
})

# export data ----
test_that("export_control", {
  expect_named(
    export_control(control = 1)
  )
})

test_that("export_object", {
  expect_named(
    export_object(keyword = "manchester united")
  )
})

test_that("export_mapping", {
  expect_named(
    export_mapping(control = 1, object = 1)
  )
})

test_that("export_global", {
  expect_named(export_global(type = "sad"))
})

test_that("export_score", {
  expect_named(
    export_score(keyword = "real madrid")
  )
})

test_that("export_doi", {
  expect_named(
    export_doi(
      control = 1,
      object = 1,
      type = "trd",
      locations = "countries"
    )
  )
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
})

test_that("remove_object", {
  expect_message(
    remove_data(table = "batch_keywords", object = 1)
  )
})

# disconnect ----
test_that("disconnect", {
  expect_message(disconnect_db())
})
