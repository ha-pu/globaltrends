# Tests for compute_score(), compute_voi(), compute_doi(), and the
# remove_data() cascade.
#
# Every test builds its own isolated database via the fixtures in helper-db.R:
#   local_score_input_db() — keyword batches + raw control/object downloads
#                            (control batch 1; object batches per argument)
#                            for US, CN, JP, and world. No computed data.
#   local_cascade_db()     — the above plus computed scores/VOI/DOI and one
#                            dt_related and dt_region row per object batch.
#
# Expected row counts: 120 months (2010-01 to 2019-12); object batch 1 has
# 4 object keywords, so scores are 4 keywords x 120 months per location.

# compute score ----------------------------------------------------------------
test_that("compute_score writes scores for all requested locations", {
  local_score_input_db()

  out <- capture_messages(compute_score(
    control = 1,
    object = 1,
    locations = c("US", "CN", "JP")
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

test_that("compute_score skips locations that already have scores", {
  local_score_input_db()
  suppressMessages(
    compute_score(control = 1, object = 1, locations = c("US", "CN", "JP"))
  )

  expect_message(
    compute_score(control = 1, object = 1, locations = c("US", "CN", "JP")),
    "No new locations to compute | control: 1 | object: 1.",
    fixed = TRUE
  )

  n <- nrow(gt.env$dt_score[
    gt.env$dt_score$batch_c == 1L & gt.env$dt_score$batch_o == 1L,
  ])
  expect_equal(n, 1440L)
})

# compute score formula --------------------------------------------------------
test_that("compute_score matches an independently computed benchmark score", {
  local_score_input_db()
  suppressMessages(
    compute_score(control = 1, object = 1, locations = c("US", "CN", "JP"))
  )

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
test_that("compute_score validates the object argument", {
  local_score_input_db()
  test_object(fun = compute_score, incl = c(1, 6:8))
})

test_that("compute_score validates the control argument", {
  local_score_input_db()
  test_control(fun = compute_score, incl = 1:5, object = 1)
})

test_that("compute_score validates the locations argument", {
  local_score_input_db()
  test_locations(fun = compute_score, object = 1)
})

test_that("compute_voi validates the object argument", {
  local_score_input_db()
  test_object(fun = compute_voi, incl = c(1, 6:8))
})

test_that("compute_voi validates the control argument", {
  local_score_input_db()
  test_control(fun = compute_voi, incl = 1:5, object = 1)
})

# compute voi ------------------------------------------------------------------
test_that("compute_voi writes world-aggregate scores", {
  local_score_input_db()

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
test_that("compute_doi writes DOI rows for computed scores", {
  local_score_input_db()
  suppressMessages(
    compute_score(control = 1, object = 1, locations = c("US", "CN", "JP"))
  )

  expect_message(
    compute_doi(control = 1, object = 1, locations = "countries"),
    "Successfully computed DOI | control: 1 | object: 1"
  )
  dt <- gt.env$dt_doi
  n <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L, ])
  expect_equal(n, 480L)
})

test_that("compute_doi skips when DOI already exists for the batch combination", {
  local_score_input_db()
  suppressMessages({
    compute_score(control = 1, object = 1, locations = c("US", "CN", "JP"))
    compute_doi(control = 1, object = 1, locations = "countries")
  })

  expect_message(
    compute_doi(control = 1, object = 1, locations = "countries"),
    "DOI already exists | control: 1 | object: 1 | locations: countries.",
    fixed = TRUE
  )
  n <- nrow(gt.env$dt_doi[
    gt.env$dt_doi$batch_c == 1L & gt.env$dt_doi$batch_o == 1L,
  ])
  expect_equal(n, 480L)
})

test_that("compute_doi messages when no score data exists", {
  local_score_input_db()

  expect_message(
    out <- compute_doi(control = 1, object = 1, locations = "countries"),
    "No score data found | control: 1 | object: 1 | locations: countries.",
    fixed = TRUE
  )
  expect_equal(out, data.frame())
  expect_equal(nrow(gt.env$dt_doi), 0L)
})

# compute doi signals ----------------------------------------------------------
test_that("compute_doi validates the object argument", {
  local_score_input_db()
  test_object(fun = compute_doi, incl = c(1, 6:8))
})

test_that("compute_doi validates the control argument", {
  local_score_input_db()
  test_control(fun = compute_doi, incl = 1:5, object = 1)
})

test_that("compute_doi validates the locations argument", {
  local_score_input_db()
  test_locations(fun = compute_doi, object = 1)
})

test_that("compute_doi rejects a locations vector of length > 1", {
  local_score_input_db()
  expect_error(
    compute_doi(object = 1, locations = letters[1:2]),
    "must have length <= 1.\nYou provided an object of length 2."
  )
})

# list dispatch ----------------------------------------------------------------
test_that("compute_score processes a list of object batches", {
  local_score_input_db(object_batches = 1:2)

  suppressMessages(
    compute_score(object = list(1, 2), control = 1, locations = c("US", "CN", "JP"))
  )

  # Expected rows for a batch: object-only keywords (not shared with the
  # control batch) x 120 months x 3 locations.
  ctrl_kws <- unique(example_control[example_control$batch == 1, ]$keyword)
  batch2_kws <- unique(example_object[
    example_object$batch_c == 1 & example_object$batch_o == 2,
  ]$keyword)
  n_scored_kws <- length(setdiff(batch2_kws, ctrl_kws))

  dt <- gt.env$dt_score
  n2 <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 2L & dt$location != "world", ])
  expect_equal(n2, n_scored_kws * 120L * 3L)

  n1 <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L & dt$location != "world", ])
  expect_equal(n1, 1440L)
})

test_that("compute_doi processes a list of object batches", {
  local_score_input_db(object_batches = 1:2)
  suppressMessages(
    compute_score(object = list(1, 2), control = 1, locations = c("US", "CN", "JP"))
  )

  suppressMessages(
    compute_doi(object = list(1, 2), control = 1, locations = "countries")
  )

  # One DOI row per (keyword, date) with score data: object-only keywords x
  # 120 months.
  ctrl_kws <- unique(example_control[example_control$batch == 1, ]$keyword)
  batch2_kws <- unique(example_object[
    example_object$batch_c == 1 & example_object$batch_o == 2,
  ]$keyword)
  n_scored_kws <- length(setdiff(batch2_kws, ctrl_kws))

  dt <- gt.env$dt_doi
  n2 <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 2L, ])
  expect_equal(n2, n_scored_kws * 120L)

  n1 <- nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L, ])
  expect_equal(n1, 480L)
})

# remove data cascade into data_related / data_region ---------------------------
test_that("remove_data on data_object cascades to score, doi, related, and region", {
  local_cascade_db(object_batches = 1:2)

  out <- capture_messages(remove_data(table = "data_object", object = 2))

  expect_match(out, "Successfully deleted object batch 2 from 'data_object'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_score'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_doi'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_related'\\.", all = FALSE)
  expect_match(out, "Successfully deleted object batch 2 from 'data_region'\\.", all = FALSE)

  # Batch 2 rows are gone everywhere ...
  expect_equal(nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 2L, ]), 0L)
  expect_equal(nrow(gt.env$dt_score[gt.env$dt_score$batch_o == 2L, ]), 0L)
  expect_equal(nrow(gt.env$dt_doi[gt.env$dt_doi$batch_o == 2L, ]), 0L)
  expect_equal(nrow(gt.env$dt_related[gt.env$dt_related$batch_o == 2L, ]), 0L)
  expect_equal(nrow(gt.env$dt_region[gt.env$dt_region$batch_o == 2L, ]), 0L)

  # ... while batch 1 rows survive.
  expect_gt(nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L, ]), 0L)
  expect_gt(nrow(gt.env$dt_score[gt.env$dt_score$batch_o == 1L, ]), 0L)
  expect_equal(nrow(gt.env$dt_related[gt.env$dt_related$batch_o == 1L, ]), 1L)
  expect_equal(nrow(gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ]), 1L)
})

# remove data ------------------------------------------------------------------
test_that("remove_data on batch_keywords with control cascades through all tables", {
  local_cascade_db()

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
  expect_equal(nrow(dt[dt$batch == 1L & dt$type == "control", ]), 0L)
  dt <- gt.env$dt_time
  expect_equal(nrow(dt[dt$batch == 1L & dt$type == "control", ]), 0L)
  expect_equal(nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L, ]), 0L)
  expect_equal(nrow(gt.env$dt_score[gt.env$dt_score$batch_c == 1L, ]), 0L)

  # Object keyword batches themselves are untouched.
  dt <- gt.env$dt_keywords
  expect_gt(nrow(dt[dt$type == "object", ]), 0L)
})

test_that("remove_data on batch_keywords with object cascades through all tables", {
  local_cascade_db()

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
  expect_equal(nrow(dt[dt$batch == 1L & dt$type == "object", ]), 0L)
  dt <- gt.env$dt_time
  expect_equal(nrow(dt[dt$batch == 1L & dt$type == "object", ]), 0L)
  expect_equal(nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L, ]), 0L)
  expect_equal(nrow(gt.env$dt_score[gt.env$dt_score$batch_o == 1L, ]), 0L)

  # Control keyword batches themselves are untouched.
  dt <- gt.env$dt_keywords
  expect_gt(nrow(dt[dt$type == "control", ]), 0L)
})

# compute score edge cases -----------------------------------------------------
# local_minimal_score_db() lives in helper-db.R.

test_that("compute_score computes hits / mapped control mass exactly", {
  # benchmark = 20/10 = 2; control mass = 10 * 2 = 20; score = 5 / 20 = 0.25
  local_minimal_score_db(ctrl_hits = 10, obj_overlap_hits = 20, obj_hits = 5)

  suppressMessages(compute_score(control = 1, object = 1, locations = "US"))

  dt <- gt.env$dt_score
  row <- dt[dt$keyword == "o1", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$score, 0.25)
})

test_that("compute_score yields zero score when control hits are all zero", {
  # zero control hits -> zero control mass -> score falls back to 0
  local_minimal_score_db(ctrl_hits = 0, obj_overlap_hits = 10, obj_hits = 5)

  suppressMessages(compute_score(control = 1, object = 1, locations = "US"))

  row <- gt.env$dt_score[gt.env$dt_score$keyword == "o1", ]
  expect_equal(row$score, 0)
})

test_that("compute_score yields zero score for NA object hits", {
  local_minimal_score_db(ctrl_hits = 10, obj_overlap_hits = 10, obj_hits = NA_real_)

  suppressMessages(compute_score(control = 1, object = 1, locations = "US"))

  row <- gt.env$dt_score[gt.env$dt_score$keyword == "o1", ]
  expect_equal(row$score, 0)
})

test_that("compute_score messages when no object data exists for the locations", {
  local_db()
  suppressMessages({
    add_control_keyword(keyword = "c1", start_date = "2020-01", end_date = "2020-01")
    add_object_keyword(keyword = "o1", start_date = "2020-01", end_date = "2020-01")
  })

  expect_message(
    out <- compute_score(control = 1, object = 1, locations = "US"),
    "No object data found | control: 1 | object: 1.",
    fixed = TRUE
  )
  expect_equal(out, 0L)
})

test_that(".resolve_score_args unlists control and defaults locations from gt.env", {
  local_db()

  args <- globaltrends:::.resolve_score_args(list(1), NULL)
  expect_equal(args$control, 1)
  expect_equal(args$locations, gt.env$countries)

  args <- globaltrends:::.resolve_score_args(1, "US")
  expect_equal(args$locations, "US")
})

test_that(".resolve_score_args falls back to the packaged countries without a session", {
  saved <- gt.env$countries
  gt.env$countries <- NULL
  withr::defer(gt.env$countries <- saved)

  args <- globaltrends:::.resolve_score_args(1, NULL)
  expect_equal(args$locations, globaltrends::countries)
})

# remove data signals ----------------------------------------------------------
test_that("remove_data validates the table argument", {
  local_db()
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

test_that("remove_data validates the control argument", {
  local_db()
  test_control(fun = remove_data, incl = 1:5, table = "data_object", object = 1)
})

test_that("remove_data validates the object argument", {
  local_db()
  test_object(fun = remove_data, incl = 1:5, table = "data_object", control = 1)
})
