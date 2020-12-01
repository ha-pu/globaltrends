# setup ------------------------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))

initialize_db()
start_db()

rm(
  tbl_control,
  tbl_doi,
  tbl_object,
  tbl_score,
  keyword_synonyms,
  keywords_control,
  keywords_object,
  time_control,
  time_object,
  envir = .GlobalEnv
)

add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
  time = "2010-01-01 2019-12-31"
)

# run downloads ----------------------------------------------------------------
test_that("download_control", {
  out <- capture_messages(download_control(control = 1, locations = countries[1:3]))

  expect_match(
    out,
    "Successfully downloaded control data | control: 1 | location: US [1/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded control data | control: 1 | location: CN [2/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded control data | control: 1 | location: JP [3/3]",
    all = FALSE
  )

  out <- filter(.tbl_control, batch == 1 & location != "world")
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

test_that("download_control_global", {
  expect_message(
    download_control_global(control = 1),
    "Successfully downloaded control data | control: 1 | location: world [1/1]"
  )
  out <- filter(.tbl_control, batch == 1 & location == "world")
  out <- collect(out)
  expect_equal(nrow(out), 600)
})

test_that("download_object", {
  out <- capture_messages(download_object(object = 1, locations = countries[1:3]))

  expect_match(
    out,
    "Successfully downloaded object data | object: 1 | control: 1 | location: US [1/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded object data | object: 1 | control: 1 | location: CN [2/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded object data | object: 1 | control: 1 | location: JP [3/3]",
    all = FALSE
  )

  out <- filter(.tbl_object, batch_o == 1 & location != "world")
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

test_that("download_object_global", {
  expect_message(
    download_object_global(object = 1),
    "Successfully downloaded object data | object: 1 | control: 1 | location: world [1/1]"
  )
  out <- filter(.tbl_object, batch_o == 1 & location == "world")
  out <- collect(out)
  expect_equal(nrow(out), 600)
})

# compute data -----------------------------------------------------------------
test_that("compute_scoring", {
  out <- capture_messages(compute_score(control = 1, object = 1, locations = countries[1:3]))

  expect_match(
    out,
    "Successfully computed search score | control: 1 | object: 1 | location: US [1/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully computed search score | control: 1 | object: 1 | location: CN [2/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully computed search score | control: 1 | object: 1 | location: JP [3/3]",
    all = FALSE
  )

  out <- filter(.tbl_score, batch_c == 1 & batch_o == 1 & location != "world")
  out <- collect(out)
  expect_equal(nrow(out), 1440)
})

test_that("compute_scoring_voi", {
  expect_message(
    compute_voi(control = 1, object = 1),
    "Successfully computed search score | control: 1 | object: 1 | location: world [1/1]",
  )
  out <- filter(.tbl_score, batch_c == 1 & batch_o == 1 & location == "world")
  out <- collect(out)
  expect_equal(nrow(out), 480)
})

test_that("compute_doi", {
  expect_message(
    compute_doi(control = 1, object = 1, locations = "countries"),
    "Successfully computed DOI | control: 1 | object: 1 [1/1]"
  )
  out <- filter(.tbl_doi, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1440)
})

# export data ------------------------------------------------------------------
test_that("export_control", {
  out <- export_control(control = 1)
  expect_equal(nrow(out), 1800)
})

test_that("export_control_global", {
  out <- export_control_global(control = 1)
  expect_equal(nrow(out), 600)
})

test_that("export_object", {
  out <- export_object(keyword = "manchester united")
  expect_equal(nrow(out), 360)
})

test_that("export_object_global", {
  out <- export_object_global(keyword = "manchester united")
  expect_equal(nrow(out), 120)
})

test_that("export_score", {
  out <- export_score(keyword = "real madrid")
  expect_equal(nrow(out), 360)
})

test_that("export_voi", {
  out <- export_voi(keyword = "real madrid")
  expect_equal(nrow(out), 120)
})

test_that("export_doi", {
  out <- export_doi(control = 1, object = 1, type = "trd", locations = "countries")
  expect_equal(nrow(out), 480)
})

# plot data --------------------------------------------------------------------
test_that("plot_score", {
  out <- export_score(keyword = "fc bayern") %>%
    filter(location %in% countries) %>%
    plot_score(type = "sad")
  expect_s3_class(out, "ggplot")
})

test_that("plot_doi_ts", {
  out <- export_doi(type = "obs", locations = "countries") %>%
    plot_doi_ts(smooth = TRUE)
  expect_s3_class(out, "ggplot")
})

test_that("plot_voi_ts", {
  out <- export_voi() %>%
    plot_voi_ts(type = "obs", smooth = TRUE)
  expect_s3_class(out, "ggplot")
})

test_that("plot_doi_box", {
  out <- export_doi(type = "sad", locations = "countries") %>%
    plot_doi_box(type = "sad")
  expect_s3_class(out, "ggplot")
})

test_that("plot_voi_box", {
  out <- export_voi() %>%
    plot_voi_box(type = "sad")
  expect_s3_class(out, "ggplot")
})

test_that("plot_voi_doi", {
  data1 <- export_doi(
    keyword = "manchester united",
    locations = "countries",
    type = "obs"
  )
  data2 <- export_voi(keyword = "manchester united")
  out <- plot_voi_doi(
    data_doi = data1,
    data_voi = data2
  )
  expect_s3_class(out, "ggplot")
})

# remove data ------------------------------------------------------------------
test_that("remove_control", {
  out <- capture_messages(remove_data(table = "batch_keywords", control = 1))

  expect_match(
    out,
    "Successfully deleted control batch 1 from 'batch_keywords'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted control batch 1 from 'data_control'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted control batch 1 from 'data_object'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted control batch 1 from 'data_score'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted control batch 1 from 'data_doi'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted control batch 1 from 'batch_time'\\.",
    all = FALSE
  )

  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "control")
  out_time <- filter(.tbl_time, batch == 1 & type == "control")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 0)
  expect_equal(nrow(out_time), 0)
})

test_that("remove_object", {
  out <- capture_messages(remove_data(table = "batch_keywords", object = 1))

  expect_match(
    out,
    "Successfully deleted object batch 1 from 'batch_keywords'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted object batch 1 from 'data_object'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted object batch 1 from 'data_score'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted object batch 1 from 'data_doi'\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully deleted object batch 1 from 'batch_time'\\.",
    all = FALSE
  )

  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "object")
  out_time <- filter(.tbl_time, batch == 1 & type == "object")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 0)
  expect_equal(nrow(out_time), 0)
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
