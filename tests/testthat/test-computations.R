# setup ------------------------------------------------------------------------
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

data <- example_control[
  example_control$batch == 1 &
  example_control$location %in% c(location_set[1:3], "world"),
]
gt.env$dt_control <- data.table::rbindlist(
  list(gt.env$dt_control, data.table::as.data.table(data)),
  use.names = TRUE
)
data <- example_object[
  example_object$batch_c == 1 &
  example_object$batch_o == 1 &
  example_object$location %in% c(location_set[1:3], "world"),
]
gt.env$dt_object <- data.table::rbindlist(
  list(gt.env$dt_object, data.table::as.data.table(data)),
  use.names = TRUE
)

# compute score ----------------------------------------------------------------
test_that("compute_score1", {
  out <- capture_messages(compute_score(
    control = 1,
    object = 1,
    locations = location_set[1:3]
  ))

  expect_match(
    out,
    "Successfully computed search scores | control: 1 | object: 1",
    all = FALSE,
    fixed = TRUE
  )

  dt <- gt.env$dt_score
  n <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L & dt$location != "world", ])
  expect_equal(n, 1440L)
})

# compute score formula --------------------------------------------------------
test_that("compute_score_formula", {
  dt <- gt.env$dt_score
  target <- as.data.frame(
    dt[dt$batch_c == 1L & dt$batch_o == 1L & dt$location == "US" & dt$keyword == "fc barcelona", ]
  )
  target <- target[order(target$date), ]
  target <- target[1, ]

  dt_int <- target$date

  obj_us <- example_object[example_object$batch_c == 1 & example_object$batch_o == 1 & example_object$location == "US", ]
  ctrl_us <- example_control[example_control$batch == 1 & example_control$location == "US", ]
  ctrl_kws <- unique(ctrl_us$keyword)

  overlap <- obj_us[obj_us$keyword %in% ctrl_kws, ]
  bm <- merge(
    overlap[, c("location", "keyword", "date", "hits")],
    ctrl_us[, c("location", "keyword", "date", "hits")],
    by = c("location", "keyword", "date"),
    suffixes = c("_o", "_c")
  )
  bm$hits_o <- ifelse(is.na(bm$hits_o) | bm$hits_o == 0, 1, as.double(bm$hits_o))
  bm$hits_c <- ifelse(is.na(bm$hits_c) | bm$hits_c == 0, 1, as.double(bm$hits_c))
  bm$ratio <- bm$hits_o / bm$hits_c

  bench <- stats::aggregate(ratio ~ location + date, data = bm, FUN = mean)
  names(bench)[3] <- "benchmark"

  ctrl_mapped <- merge(
    ctrl_us[, c("location", "keyword", "date", "hits")],
    bench, by = c("location", "date")
  )
  ctrl_mapped$hits_mapped <- as.double(ctrl_mapped$hits) * ctrl_mapped$benchmark

  ctrl_mass <- stats::aggregate(
    hits_mapped ~ location + date, data = ctrl_mapped, FUN = sum
  )
  names(ctrl_mass)[3] <- "hits_c"

  obj_row <- obj_us[obj_us$keyword == "fc barcelona" & obj_us$date == dt_int, ]
  obj_row <- merge(obj_row, ctrl_mass, by = c("location", "date"))

  expected_score <- if (is.na(obj_row$hits_c) || obj_row$hits_c <= 0) {
    0
  } else {
    ifelse(is.na(obj_row$hits), 0, as.double(obj_row$hits)) / obj_row$hits_c
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
    "Successfully computed search scores | control: 1 | object: 1",
    fixed = TRUE
  )
  dt <- gt.env$dt_score
  n <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L & dt$location == "world", ])
  expect_equal(n, 480L)
})

# compute doi ------------------------------------------------------------------
test_that("compute_doi1", {
  expect_message(
    compute_doi(control = 1, object = 1, locations = "countries"),
    "Successfully computed DOI | control: 1 | object: 1"
  )
  dt <- gt.env$dt_doi
  n <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L, ])
  expect_equal(n, 480L)
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
kws2 <- example_keywords[example_keywords$type == "object" & example_keywords$batch == 2, ]$keyword
add_object_keyword(keyword = kws2, start_date = "2010-01", end_date = "2019-12")
data <- example_object[
  example_object$batch_c == 1 &
  example_object$batch_o == 2 &
  example_object$location %in% c(location_set[1:3], "world"),
]
gt.env$dt_object <- data.table::rbindlist(
  list(gt.env$dt_object, data.table::as.data.table(data)),
  use.names = TRUE
)

# list dispatch ----------------------------------------------------------------
test_that("compute_score_list", {
  compute_score(object = list(1, 2), control = 1, locations = location_set[1:3])

  dt <- gt.env$dt_score
  n <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 2L & dt$location != "world", ])
  expect_gt(n, 0L)

  n1 <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L & dt$location != "world", ])
  expect_equal(n1, 1440L)
})

test_that("compute_doi_list", {
  compute_doi(object = list(1, 2), control = 1, locations = "countries")

  dt <- gt.env$dt_doi
  n <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 2L, ])
  expect_gt(n, 0L)

  n1 <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L, ])
  expect_equal(n1, 480L)
})

# remove data cascade into data_related / data_region -------------------------
gt.env$dt_related <- data.table::rbindlist(list(
  gt.env$dt_related,
  data.table::data.table(
    term = "fc barcelona", topic = 0L, rising = 0L,
    location = "world", start_date = as.Date("2019-01-01"),
    end_date = as.Date("2019-12-31"),
    related_term = "barcelona", hits = 100.0, batch_o = 2L
  )
), use.names = TRUE)

gt.env$dt_region <- data.table::rbindlist(list(
  gt.env$dt_region,
  data.table::data.table(
    term = "fc barcelona", location = "world",
    start_date = as.Date("2019-01-01"), end_date = as.Date("2019-12-31"),
    region_code = "ES-CT", region_name = "Catalonia", hits = 100.0, batch_o = 2L
  )
), use.names = TRUE)

test_that("remove_data7", {
  out <- capture_messages(remove_data(table = "data_object", object = 2))

  expect_match(out, "Successfully deleted object batch 2 from 'data_object'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_score'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_doi'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_related'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_region'\\.", all = FALSE)

  rel <- nrow(gt.env$dt_related[gt.env$dt_related$batch_o == 2L, ])
  reg <- nrow(gt.env$dt_region[gt.env$dt_region$batch_o == 2L, ])
  expect_equal(rel, 0L)
  expect_equal(reg, 0L)
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

  dt <- gt.env$dt_keywords
  n <- nrow(dt[dt$batch == 1L & dt$type == "control", ])
  expect_equal(n, 0L)
  dt <- gt.env$dt_time
  n <- nrow(dt[dt$batch == 1L & dt$type == "control", ])
  expect_equal(n, 0L)
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

  dt <- gt.env$dt_keywords
  n <- nrow(dt[dt$batch == 1L & dt$type == "object", ])
  expect_equal(n, 0L)
  dt <- gt.env$dt_time
  n <- nrow(dt[dt$batch == 1L & dt$type == "object", ])
  expect_equal(n, 0L)
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

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
