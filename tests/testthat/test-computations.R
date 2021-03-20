# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
  time = "2010-01-01 2019-12-31"
)

data <- filter(example_control, batch == 1 & location %in% c(countries[1:3], "world"))
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o == 1 & location %in% c(countries[1:3], "world"))
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)

# compute score ----------------------------------------------------------------
test_that("compute_score1", {
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

# compute score signals --------------------------------------------------------
test_that("compute_score2", {
  expect_error(
    compute_score(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_score(object = "A"),
    "no applicable method"
  )
  expect_error(
    compute_score(object = TRUE),
    "no applicable method"
  )
  expect_error(
    compute_score(object = sum),
    "no applicable method"
  )
})

test_that("compute_score3", {
  expect_error(
    compute_score(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_score(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_score(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_score(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_score(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("compute_score4", {
  expect_error(
    compute_score(object = 1, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_score(object = 1, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    compute_score(object = 1, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
})

test_that("compute_score5", {
  expect_error(
    compute_voi(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_voi(object = "A"),
    "no applicable method"
  )
  expect_error(
    compute_voi(object = TRUE),
    "no applicable method"
  )
  expect_error(
    compute_voi(object = sum),
    "no applicable method"
  )
})

test_that("compute_score6", {
  expect_error(
    compute_voi(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_voi(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_voi(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_voi(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_voi(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

# compute voi ------------------------------------------------------------------
test_that("compute_voi1", {
  expect_message(
    compute_voi(control = 1, object = 1),
    "Successfully computed search score | control: 1 | object: 1 | location: world [1/1]",
  )
  out <- filter(.tbl_score, batch_c == 1 & batch_o == 1 & location == "world")
  out <- collect(out)
  expect_equal(nrow(out), 480)
})

# compute doi ------------------------------------------------------------------
test_that("compute_doi1", {
  expect_message(
    compute_doi(control = 1, object = 1, locations = "countries"),
    "Successfully computed DOI | control: 1 | object: 1 [1/1]"
  )
  out <- filter(.tbl_doi, batch_c == 1 & batch_o == 1)
  out <- collect(out)
  expect_equal(nrow(out), 1440)
})


# compute doi signals ----------------------------------------------------------
test_that("compute_doi2", {
  expect_error(
    compute_doi(object = 1.5),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_doi(object = "A"),
    "no applicable method"
  )
  expect_error(
    compute_doi(object = TRUE),
    "no applicable method"
  )
  expect_error(
    compute_doi(object = sum),
    "no applicable method"
  )
})

test_that("compute_doi3", {
  expect_error(
    compute_doi(control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    compute_doi(control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_doi(control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_doi(control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    compute_doi(control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("compute_doi4", {
  expect_error(
    compute_doi(object = 1, locations = 1),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_doi(object = 1, locations = TRUE),
    "'locations' must be object of type character.\nYou provided an object of type logical"
  )
  expect_error(
    compute_doi(object = 1, locations = sum),
    "'locations' must be object of type character.\nYou provided an object of type builtin"
  )
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
  
  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "control")
  out_time <- filter(.tbl_time, batch == 1 & type == "control")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 0)
  expect_equal(nrow(out_time), 0)
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
  
  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "object")
  out_time <- filter(.tbl_time, batch == 1 & type == "object")
  out_keywords <- collect(out_keywords)
  out_time <- collect(out_time)
  expect_equal(nrow(out_keywords), 0)
  expect_equal(nrow(out_time), 0)
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
  expect_error(
    remove_data(table = "data_object", control = 1.5, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    remove_data(table = "data_object", control = "A", object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", control = TRUE, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", control = sum, object = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", control = 1:5, object = 1),
    "'control' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("remove_data5", {
  expect_error(
    remove_data(table = "data_object", object = 1.5, control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
  )
  expect_error(
    remove_data(table = "data_object", object = "A", control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", object = TRUE, control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", object = sum, control = 1),
    "Batch number must be object of type integer.\nYou provided a non-integer value."
  )
  expect_error(
    remove_data(table = "data_object", object = 1:5, control = 1),
    "'object' must be object of length 1.\nYou provided an object of length 5."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
