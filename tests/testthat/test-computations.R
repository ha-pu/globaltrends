# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

location_set <- c("US", "CN", "JP")

# enter data -------------------------------------------------------------------
add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  start_date = "2010-01",
  end_date = "2019-12"
)

add_object_keyword(
  keyword = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
  start_date = "2010-01",
  end_date = "2019-12"
)

data <- filter(
  example_control,
  batch == 1 & location %in% c(location_set[1:3], "world")
)
dbAppendTable(gt.env$globaltrends_db, "data_control", data)
data <- filter(
  example_object,
  batch_c == 1 & batch_o == 1 & location %in% c(location_set[1:3], "world")
)
dbAppendTable(gt.env$globaltrends_db, "data_object", data)

# compute score ----------------------------------------------------------------
test_that("compute_score1", {
  out <- capture_messages(compute_score(
    control = 1,
    object = 1,
    locations = location_set[1:3]
  ))

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

  out <- filter(
    gt.env$tbl_score,
    batch_c == 1 & batch_o == 1 & location != "world"
  )
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 1440)
})

# compute score signals --------------------------------------------------------
test_that("compute_score2", {
  test_object(fun = compute_score, incl = c(1, 6:8))
})

test_that("compute_score3", {
  test_control(fun = compute_score, incl = 1:5, object = 1)
})

test_that("compute_score4", {
  test_locations(fun = compute_score, object = 1)
})

test_that("compute_score5", {
  test_object(fun = compute_voi, incl = c(1, 6:8))
})

test_that("compute_score6", {
  test_control(fun = compute_voi, incl = 1:5, object = 1)
})

# compute voi ------------------------------------------------------------------
test_that("compute_voi1", {
  expect_message(
    compute_voi(control = 1, object = 1),
    "Successfully computed search score | control: 1 | object: 1 | location: world [1/1]",
  )
  out <- filter(
    gt.env$tbl_score,
    batch_c == 1 & batch_o == 1 & location == "world"
  )
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 480)
})

# compute doi ------------------------------------------------------------------
test_that("compute_doi1", {
  expect_message(
    compute_doi(control = 1, object = 1, locations = "countries"),
    "Successfully computed DOI | control: 1 | object: 1 [1/1]"
  )
  out <- filter(gt.env$tbl_doi, batch_c == 1 & batch_o == 1)
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 480)
})

# compute doi signals ----------------------------------------------------------
test_that("compute_doi2", {
  test_object(fun = compute_doi, incl = c(1, 6:8))
})

test_that("compute_doi3", {
  test_control(fun = compute_doi, incl = 1:5, object = 1)
})

test_that("compute_doi4", {
  test_locations(fun = compute_doi, object = 1)
})

test_that("compute_doi5", {
  expect_error(
    compute_doi(object = 1, locations = letters[1:2]),
    "'locations' must be object of length 1.\nYou provided an object of length 2."
  )
})

# remove data ------------------------------------------------------------------
test_that("remove_data1", {
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

  out <- filter(gt.env$tbl_keywords, batch == 1 & type == "control")
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 0)
  out <- filter(gt.env$tbl_time, batch == 1 & type == "control")
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 0)
})

test_that("remove_data2", {
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

  out <- filter(gt.env$tbl_keywords, batch == 1 & type == "object")
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 0)
  out <- filter(gt.env$tbl_time, batch == 1 & type == "object")
  out <- count(out)
  out <- collect(out)
  expect_equal(out$n, 0)
})

# remove data signals ----------------------------------------------------------
test_that("remove_data3", {
  expect_error(
    remove_data(table = 1),
    "'table' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    remove_data(table = "A"),
    "'table' must be either 'batch_keywords', 'batch_time', 'data_control', 'data_object', 'data_score', or 'data_doi'.\nYou provided A."
  )
  expect_error(
    remove_data(table = TRUE),
    "'table' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    remove_data(table = sum),
    "'table' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    remove_data(table = c("data_object", "data_control")),
    "'table' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("remove_data4", {
  test_control(fun = remove_data, incl = 1:5, table = "data_object", object = 1)
})

test_that("remove_data5", {
  test_object(fun = remove_data, incl = 1:5, table = "data_object", control = 1)
})

# remove data vacuum -----------------------------------------------------------
test_that("remove_data6", {
  size_t0 <- file.size("db/globaltrends_db.sqlite")
  expect_message(vacuum_data(), "Vacuum completed successfully.")
  size_t1 <- file.size("db/globaltrends_db.sqlite")
  if (size_t0 == 483328) expect_true(size_t0 > size_t1)
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
