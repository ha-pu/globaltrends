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

# compute score formula --------------------------------------------------------
test_that("compute_score_formula", {
  target <- gt.env$tbl_score |>
    filter(batch_c == 1L, batch_o == 1L, location == "US", keyword == "fc barcelona") |>
    collect() |>
    arrange(date) |>
    slice(1)

  dt_int <- as.integer(target$date)

  obj_us <- filter(example_object, batch_c == 1, batch_o == 1, location == "US")
  ctrl_us <- filter(example_control, batch == 1, location == "US")
  ctrl_kws <- distinct(ctrl_us, keyword)

  bench <- obj_us |>
    inner_join(ctrl_kws, by = "keyword") |>
    inner_join(ctrl_us, by = c("location", "keyword", "date"), suffix = c("_o", "_c")) |>
    mutate(
      hits_o = if_else(coalesce(as.double(hits_o), 0) == 0, 1, as.double(hits_o)),
      hits_c = if_else(coalesce(as.double(hits_c), 0) == 0, 1, as.double(hits_c)),
      ratio  = hits_o / hits_c
    ) |>
    summarise(benchmark = mean(ratio, na.rm = TRUE), .by = c(location, date))

  ctrl_mass <- ctrl_us |>
    inner_join(bench, by = c("location", "date")) |>
    mutate(hits_mapped = as.double(hits) * coalesce(benchmark, 0)) |>
    summarise(hits_c = sum(hits_mapped, na.rm = TRUE), .by = c(location, date))

  obj_row <- obj_us |>
    filter(keyword == "fc barcelona", date == dt_int) |>
    left_join(ctrl_mass, by = c("location", "date"))

  expected_score <- if (is.na(obj_row$hits_c) || obj_row$hits_c <= 0) {
    0
  } else {
    coalesce(as.double(obj_row$hits), 0) / obj_row$hits_c
  }

  expect_equal(target$score, expected_score, tolerance = 1e-6)
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
    "must have length <= 1.\nYou provided an object of length 2."
  )
})

# list dispatch setup ----------------------------------------------------------
kws2 <- filter(example_keywords, type == "object", batch == 2)$keyword
add_object_keyword(keyword = kws2, start_date = "2010-01", end_date = "2019-12")
data <- filter(
  example_object,
  batch_c == 1 & batch_o == 2 & location %in% c(location_set[1:3], "world")
)
dbAppendTable(gt.env$globaltrends_db, "data_object", data)

# list dispatch ----------------------------------------------------------------
test_that("compute_score_list", {
  compute_score(object = list(1, 2), control = 1, locations = location_set[1:3])

  out <- filter(gt.env$tbl_score, batch_c == 1L, batch_o == 2L, location != "world")
  out <- count(out)
  out <- collect(out)
  expect_gt(out$n, 0)

  out1 <- filter(gt.env$tbl_score, batch_c == 1L, batch_o == 1L, location != "world")
  out1 <- count(out1)
  out1 <- collect(out1)
  expect_equal(out1$n, 1440)
})

test_that("compute_doi_list", {
  compute_doi(object = list(1, 2), control = 1, locations = "countries")

  out <- filter(gt.env$tbl_doi, batch_c == 1L, batch_o == 2L)
  out <- count(out)
  out <- collect(out)
  expect_gt(out$n, 0)

  out1 <- filter(gt.env$tbl_doi, batch_c == 1L, batch_o == 1L)
  out1 <- count(out1)
  out1 <- collect(out1)
  expect_equal(out1$n, 480)
})

# remove data cascade into data_related / data_region -------------------------
dbAppendTable(
  gt.env$globaltrends_db,
  "data_related",
  data.frame(
    term = "fc barcelona", topic = FALSE, rising = FALSE,
    location = "world", start_date = as.Date("2019-01-01"),
    end_date = as.Date("2019-12-31"),
    related_term = "barcelona", hits = 100.0, batch_o = 2L
  )
)
dbAppendTable(
  gt.env$globaltrends_db,
  "data_region",
  data.frame(
    term = "fc barcelona", location = "world",
    start_date = as.Date("2019-01-01"), end_date = as.Date("2019-12-31"),
    region_code = "ES-CT", region_name = "Catalonia", hits = 100.0, batch_o = 2L
  )
)

test_that("remove_data7", {
  out <- capture_messages(remove_data(table = "data_object", object = 2))

  expect_match(out, "Successfully deleted object batch 2 from 'data_object'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_score'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_doi'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_related'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_region'\\.", all = FALSE)

  rel <- filter(gt.env$tbl_related, batch_o == 2L) |>
    count() |>
    collect()
  reg <- filter(gt.env$tbl_region, batch_o == 2L) |>
    count() |>
    collect()
  expect_equal(rel$n, 0L)
  expect_equal(reg$n, 0L)
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
    "must be of type character.\nYou provided an object of type double."
  )
  expect_error(
    remove_data(table = "A"),
    "`table` must be one of:"
  )
  expect_error(
    remove_data(table = TRUE),
    "must be of type character.\nYou provided an object of type logical."
  )
  expect_error(
    remove_data(table = sum),
    "must be of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    remove_data(table = c("data_object", "data_control")),
    "must have length <= 1.\nYou provided an object of length 2."
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
  expect_message(vacuum_data(), "Vacuum completed successfully\\.")
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
