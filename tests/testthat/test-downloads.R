# Tests for download_control(), download_object(), and their *_global() wrappers.
#
# Structure:
#   Guard conditions     — purely offline; error/skip paths that fire before any
#                          API call reaches .get_trend().
#   Happy path (mocked)  — offline-safe; .get_trend() is stubbed with
#                          local_mocked_bindings() so these run without internet
#                          access and without Google Trends credentials.
#   Input validation     — purely offline; wrong argument types and values.
#   Live network tests   — opt-in via GLOBALTRENDS_LIVE_TESTS=1 (see
#                          skip_if_no_live_api() in helper-skips.R); hit the
#                          real API for true end-to-end verification.
#
# The local_db() helper (from helper-db.R) provides fully isolated database
# state for every test. Cleanup is handled automatically via withr::defer().

# Shared fixtures (setup_keywords, make_trend_data, location_set) live in
# helper-fixtures.R; validation batteries (test_control etc.) in
# helper-validation.R; skip gates (.setup_python_api etc.) in helper-skips.R.

# ── Guard conditions (no network) ────────────────────────────────────────────

test_that("download_object emits skip message when no control baseline exists for location", {
  local_db()
  setup_keywords()

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set[[1]])
  )

  expect_match(
    out,
    "Skipped object download \\(missing control baseline\\) \\| object: 1 \\| control: 1 \\| location: US",
    all = FALSE
  )
})

test_that("download_control skips location already present in data_control", {
  local_db()
  setup_keywords()
  suppressMessages(
    gt.env$dt_control <- data.table::rbindlist(list(
      gt.env$dt_control,
      data.table::as.data.table(example_control[example_control$batch == 1 & example_control$location == "US", ])
    ), use.names = TRUE)
  )

  out <- capture_messages(
    download_control(control = 1, locations = "US")
  )

  expect_match(out, "No new locations to download \\| control: 1", all = FALSE)
})

test_that("download_control errors on unknown batches or missing metadata", {
  local_db()
  setup_keywords()

  expect_error(
    download_control(control = 99, locations = "US"),
    "No keywords found for control batch 99.",
    fixed = TRUE
  )

  saved_kw <- gt.env$keywords_control
  gt.env$keywords_control <- NULL
  withr::defer(gt.env$keywords_control <- saved_kw)
  expect_error(
    download_control(control = 1, locations = "US"),
    "Control batch metadata not found in `gt.env`. Run `start_db()` first.",
    fixed = TRUE
  )
})

test_that("download_control messages when the API returns no data [mocked]", {
  local_db()
  setup_keywords()
  local_mocked_bindings(.get_trend = function(...) NULL, .package = "globaltrends")

  expect_message(
    download_control(control = 1, locations = "US"),
    "No data returned | control: 1 | location: US [1/1]",
    fixed = TRUE
  )
  expect_equal(nrow(gt.env$dt_control), 0L)
})

test_that("download_object errors on unknown batches or missing metadata", {
  local_db()
  setup_keywords()

  expect_error(
    download_object(object = 99, control = 1, locations = "US"),
    "No keywords found for object batch 99.",
    fixed = TRUE
  )

  saved_kw <- gt.env$keywords_object
  gt.env$keywords_object <- NULL
  withr::defer(gt.env$keywords_object <- saved_kw)
  expect_error(
    download_object(object = 1, control = 1, locations = "US"),
    "Object batch metadata not found in `gt.env`. Run `start_db()` first.",
    fixed = TRUE
  )
})

test_that("download_object errors when the control baseline has no signal", {
  local_db()
  setup_keywords()
  # Control baseline exists but every keyword has zero hits.
  seed_table("dt_control", data.frame(
    location = "US",
    keyword  = c("gmail", "map", "translate", "wikipedia", "youtube"),
    date     = as.Date("2010-01-01"),
    hits     = 0,
    batch    = 1L
  ))

  expect_error(
    download_object(object = 1, control = 1, locations = "US"),
    "Too little signal in control batch 1 for location US.",
    fixed = TRUE
  )
})

test_that("download_object errors when no control keyword yields usable signal [mocked]", {
  local_db()
  setup_keywords()
  seed_table("dt_control", data.frame(
    location = "US",
    keyword  = c("gmail", "map", "translate", "wikipedia", "youtube"),
    date     = as.Date("2010-01-01"),
    hits     = 50,
    batch    = 1L
  ))
  # Every download returns zero hits, so no control keyword passes the
  # positive-signal check.
  local_mocked_bindings(
    .get_trend = function(location = NULL, term, start_date, end_date) {
      out <- make_trend_data(location, term, start_date, end_date)
      out$hits <- 0
      out
    },
    .package = "globaltrends"
  )

  expect_error(
    download_object(object = 1, control = 1, locations = "US"),
    "Download failed: no control keyword produced usable signal for object batch 1",
    fixed = TRUE
  )
})

# ── Happy path — mocked (offline-safe) ───────────────────────────────────────
#
# .get_trend() is stubbed with make_trend_data() for every test in this section.
# Row-count expectations follow from: 5 keywords × 120 months (2010-01–2019-12)
# per location. download_object stores all (1 control + 4 object) = 5 terms.

test_that("download_control writes correct rows and emits per-location messages [mocked]", {
  local_db()
  setup_keywords()
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  out <- capture_messages(
    download_control(control = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )
  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: CN \\[2/3\\]",
    all = FALSE
  )
  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: JP \\[3/3\\]",
    all = FALSE
  )

  # 5 keywords × 120 months × 3 locations = 1 800 rows
  n <- nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L, ])
  expect_equal(n, 1800)
})

test_that("download_control skips all locations on a second call [mocked]", {
  local_db()
  setup_keywords()
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  suppressMessages(download_control(control = 1, locations = location_set))

  out <- capture_messages(
    download_control(control = 1, locations = location_set[[1]])
  )

  expect_match(out, "No new locations to download \\| control: 1", all = FALSE)
})

test_that("download_control_global writes 600 rows for the world aggregate [mocked]", {
  local_db()
  setup_keywords()
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  out <- capture_messages(download_control_global(control = 1))

  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: world \\[1/1\\]",
    all = FALSE
  )
  # 5 keywords × 120 months = 600 rows
  n <- nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L & gt.env$dt_control$location == "world", ])
  expect_equal(n, 600)
})

test_that("download_object writes correct rows and selects control keyword [mocked]", {
  local_db()
  setup_keywords()
  # Pre-insert control baseline so download_object can rank control keywords.
  suppressMessages(
    gt.env$dt_control <- data.table::rbindlist(list(
      gt.env$dt_control,
      data.table::as.data.table(example_control[example_control$batch == 1 & example_control$location %in% location_set, ])
    ), use.names = TRUE)
  )
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )
  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: CN \\[2/3\\]",
    all = FALSE
  )
  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: JP \\[3/3\\]",
    all = FALSE
  )

  # (1 control + 4 object) terms × 120 months × 3 locations = 1 800 rows
  n <- nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L, ])
  expect_equal(n, 1800)
})

test_that("download_object skips (batch_c, batch_o, location) already present [mocked]", {
  local_db()
  setup_keywords()
  suppressMessages(
    gt.env$dt_control <- data.table::rbindlist(list(
      gt.env$dt_control,
      data.table::as.data.table(example_control[example_control$batch == 1 & example_control$location %in% location_set, ])
    ), use.names = TRUE)
  )
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  suppressMessages(download_object(object = 1, control = 1, locations = location_set))

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set[[1]])
  )

  expect_match(
    out,
    "No new locations to download \\| object: 1 \\| control: 1",
    all = FALSE
  )
})

test_that("download_object_global writes 600 rows for the world aggregate [mocked]", {
  local_db()
  setup_keywords()
  suppressMessages(
    gt.env$dt_control <- data.table::rbindlist(list(
      gt.env$dt_control,
      data.table::as.data.table(example_control[example_control$batch == 1 & example_control$location == "world", ])
    ), use.names = TRUE)
  )
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  out <- capture_messages(download_object_global(object = 1, control = 1))

  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: world \\[1/1\\]",
    all = FALSE
  )
  # (1 control + 4 object) terms × 120 months = 600 rows
  n <- nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L & gt.env$dt_object$location == "world", ])
  expect_equal(n, 600)
})

test_that("download_control_global uses the Research API world path when py_setup is TRUE", {
  local_db()
  setup_keywords()
  local_counter_state()
  local_py_state()

  out <- capture_messages(download_control_global(control = 1))

  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: world \\[1/1\\]",
    all = FALSE
  )
  # The fake Research API backend returns one point per term: 5 keywords.
  dt <- gt.env$dt_control[gt.env$dt_control$batch == 1L, ]
  expect_equal(nrow(dt), 5L)
  expect_true(all(dt$location == "world"))
  expect_equal(gt.env$api_calls, 1L)
})

test_that("download_object_global uses the Research API world path when py_setup is TRUE", {
  local_db()
  setup_keywords()
  local_counter_state()
  local_py_state()
  seed_table("dt_control", data.frame(
    location = "world",
    keyword  = c("gmail", "map", "translate", "wikipedia", "youtube"),
    date     = as.Date("2010-01-01"),
    hits     = 50,
    batch    = 1L
  ))

  out <- capture_messages(download_object_global(object = 1, control = 1))

  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: world \\[1/1\\]",
    all = FALSE
  )
  # 1 control + 4 object keywords, one point each.
  dt <- gt.env$dt_object[gt.env$dt_object$batch_o == 1L, ]
  expect_equal(nrow(dt), 5L)
  expect_true(all(dt$location == "world"))
})

# ── Input validation (no network) ────────────────────────────────────────────

test_that("download_control errors on invalid control type or value", {
  local_db()
  setup_keywords()
  test_control(fun = download_control, incl = c(1, 6:8))
})

test_that("download_control errors on invalid locations type", {
  local_db()
  setup_keywords()
  test_locations(fun = download_control, control = 1)
})

test_that("download_object errors on invalid object type or value", {
  local_db()
  setup_keywords()
  test_object(fun = download_object, incl = c(1, 6:8), control = 1)
})

test_that("download_object errors on invalid control type or value", {
  local_db()
  setup_keywords()
  test_control(fun = download_object, incl = 1:5, object = 1)
})

test_that("download_object errors on invalid locations type", {
  local_db()
  setup_keywords()
  test_locations(fun = download_object, object = 1, control = 1)
})

# ── download_region / download_related — mocked (offline-safe) ───────────────
#
# These exercise the dispatch, dedup, and progress-message logic of the
# Research-API-only download functions by activating the backend with
# local_py_state() and faking gt.env$query_region / gt.env$query_terms.
# setup_keywords() registers 4 object keywords for batch 1.

test_that("download_region errors when the Python backend is not initialized", {
  local_db()
  setup_keywords()
  local_py_setup_state()
  gt.env$py_setup <- FALSE

  expect_error(
    download_region(object = 1, locations = location_set),
    "Python backend is not initialized. Run `initialize_python()` first.",
    fixed = TRUE
  )
})

test_that("download_region writes rows per location and skips on re-download [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_region(function(terms, start_date, end_date, geo, api_key) {
    list(regions = list(
      list(regionCode = "R1", regionName = "Region One", value = 80),
      list(regionCode = "R2", regionName = "Region Two", value = 20)
    ))
  })

  out <- capture_messages(download_region(object = 1, locations = location_set))

  expect_match(
    out,
    "Downloaded region data \\| object: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )
  expect_match(
    out,
    "Downloaded region data \\| object: 1 \\| location: JP \\[3/3\\]",
    all = FALSE
  )

  # 4 object keywords x 2 regions x 3 locations = 24 rows
  dt <- gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ]
  expect_equal(nrow(dt), 24L)
  expect_setequal(unique(dt$location), location_set)
  expect_setequal(unique(dt$region_code), c("R1", "R2"))

  # Second call: everything already present.
  expect_message(
    download_region(object = 1, locations = location_set[[1]]),
    "No new locations to download | object: 1.",
    fixed = TRUE
  )
})

test_that("download_region_global writes the world aggregate [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_region(function(...) {
    list(regions = list(list(regionCode = "R1", regionName = "Region One", value = 100)))
  })

  expect_message(
    download_region_global(object = 1),
    "Downloaded region data \\| object: 1 \\| location: world \\[1/1\\]"
  )

  dt <- gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ]
  expect_equal(nrow(dt), 4L)
  expect_true(all(dt$location == "world"))
})

test_that("download_region messages when a location returns no usable data [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  # NA-term rows are what .get_region() produces for failed requests; they
  # must be filtered out rather than written.
  local_mocked_bindings(
    .get_region = function(...) {
      data.frame(
        term = NA_character_, location = NA_character_,
        start_date = as.Date(NA), end_date = as.Date(NA),
        region_code = NA_character_, region_name = NA_character_,
        hits = NA_real_, stringsAsFactors = FALSE
      )
    },
    .package = "globaltrends"
  )

  expect_message(
    download_region(object = 1, locations = "US"),
    "No region data returned | object: 1 | location: US [1/1]",
    fixed = TRUE
  )
  expect_equal(nrow(gt.env$dt_region), 0L)
})

test_that("download_region errors on unknown batches or missing metadata", {
  local_db()
  setup_keywords()
  local_py_state()

  expect_error(
    download_region(object = 99, locations = "US"),
    "No keywords found for object batch 99.",
    fixed = TRUE
  )

  saved_kw <- gt.env$keywords_object
  gt.env$keywords_object <- NULL
  withr::defer(gt.env$keywords_object <- saved_kw)
  expect_error(
    download_region(object = 1, locations = "US"),
    "Object batch metadata not found in `gt.env`. Run `start_db()` first.",
    fixed = TRUE
  )
})

test_that("download_related errors when the Python backend is not initialized", {
  local_db()
  setup_keywords()
  local_py_setup_state()
  gt.env$py_setup <- FALSE

  expect_error(
    download_topics(object = 1, locations = location_set),
    "Python backend is not initialized. Run `initialize_python()` first.",
    fixed = TRUE
  )
})

test_that("download_related requires logical topic and rising flags", {
  local_db()
  setup_keywords()
  local_py_state()

  expect_error(
    download_related(object = 1, locations = "US", topic = NULL, rising = FALSE),
    "`topic` must be of type logical"
  )
  expect_error(
    download_related(object = 1, locations = "US", topic = TRUE, rising = "no"),
    "`rising` must be of type logical"
  )
})

test_that("download_topics writes rows per location and skips on re-download [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_terms(function(terms, start_date, end_date, geo, api_key, topic, rising) {
    list(item = list(
      list(title = "related one", value = 100),
      list(title = "related two", value = 40)
    ))
  })

  out <- capture_messages(download_topics(object = 1, locations = location_set))

  expect_match(
    out,
    "Downloaded related data \\| object: 1 \\| location: US \\| topic: TRUE \\| rising: FALSE \\[1/3\\]",
    all = FALSE
  )

  # 4 object keywords x 2 related terms x 3 locations = 24 rows
  dt <- gt.env$dt_related[gt.env$dt_related$batch_o == 1L, ]
  expect_equal(nrow(dt), 24L)
  expect_true(all(dt$topic == 1L))
  expect_true(all(dt$rising == 0L))
  expect_setequal(unique(dt$related_term), c("related one", "related two"))

  expect_message(
    download_topics(object = 1, locations = location_set[[1]]),
    "No new locations to download | object: 1 | topic: TRUE | rising: FALSE.",
    fixed = TRUE
  )
})

test_that("download_themes_rising_global stores the world aggregate with correct flags [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_terms(function(...) {
    list(item = list(list(title = "rising theme", value = 100)))
  })

  expect_message(
    download_themes_rising_global(object = 1),
    "Downloaded related data \\| object: 1 \\| location: world \\| topic: FALSE \\| rising: TRUE \\[1/1\\]"
  )

  dt <- gt.env$dt_related[gt.env$dt_related$batch_o == 1L, ]
  expect_equal(nrow(dt), 4L)
  expect_true(all(dt$location == "world"))
  expect_true(all(dt$topic == 0L))
  expect_true(all(dt$rising == 1L))
})

test_that("download_related messages when the API returns no data [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_mocked_bindings(
    .get_related = function(...) NULL,
    .package = "globaltrends"
  )

  expect_message(
    download_topics(object = 1, locations = "US"),
    "No data returned | object: 1 | location: US | topic: TRUE | rising: FALSE [1/1]",
    fixed = TRUE
  )
  expect_equal(nrow(gt.env$dt_related), 0L)
})

test_that("every download_related wrapper sets the right topic/rising/location combination [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_terms(function(...) {
    list(item = list(list(title = "related one", value = 100)))
  })

  wrappers <- list(
    list(fun = function() download_themes(object = 1, locations = "US"),
         topic = 0L, rising = 0L, location = "US"),
    list(fun = function() download_topics_rising(object = 1, locations = "CN"),
         topic = 1L, rising = 1L, location = "CN"),
    list(fun = function() download_themes_rising(object = 1, locations = "JP"),
         topic = 0L, rising = 1L, location = "JP"),
    list(fun = function() download_topics_global(object = 1),
         topic = 1L, rising = 0L, location = "world"),
    list(fun = function() download_themes_global(object = 1),
         topic = 0L, rising = 0L, location = "world"),
    list(fun = function() download_topics_rising_global(object = 1),
         topic = 1L, rising = 1L, location = "world")
  )

  for (w in wrappers) {
    before <- nrow(gt.env$dt_related)
    suppressMessages(w$fun())
    # setkey() inside download_related() re-sorts the table, so locate the
    # new rows by their distinct (topic, rising, location) triple instead of
    # by position. 4 object keywords x 1 related term = 4 rows per call.
    dt <- gt.env$dt_related
    new_rows <- dt[
      dt$topic == w$topic & dt$rising == w$rising & dt$location == w$location,
    ]
    expect_equal(nrow(new_rows), 4L)
    expect_equal(nrow(dt), before + 4L)
  }
})

test_that("download_region processes list input sequentially [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_region(function(...) {
    list(regions = list(list(regionCode = "R1", regionName = "Region One", value = 100)))
  })

  suppressMessages(download_region(object = list(1), locations = "US"))
  expect_equal(nrow(gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ]), 4L)

  # Vector input delegates to the list method and dedups the repeat.
  out <- capture_messages(download_region(object = c(1, 1), locations = "US"))
  expect_match(out, "No new locations to download \\| object: 1\\.", all = TRUE)
})

test_that("download_related delegates vector input to the list method [mocked]", {
  local_db()
  setup_keywords()
  local_py_state()
  local_query_terms(function(...) {
    list(item = list(list(title = "related one", value = 100)))
  })

  suppressMessages(download_topics(object = 1, locations = "US"))

  # c(1, 1): the repeated batch is fully deduplicated on the second pass.
  out <- capture_messages(download_topics(object = c(1, 1), locations = "US"))
  expect_match(
    out,
    "No new locations to download \\| object: 1 \\| topic: TRUE \\| rising: FALSE\\.",
    all = TRUE
  )
})

# ── Live network integration (opt-in) ────────────────────────────────────────
#
# These tests exercise the real gtrendsR / Research API backend. They are
# skipped on CRAN, when offline, and unless GLOBALTRENDS_LIVE_TESTS=1 is set.
# Run them locally after confirming credentials are configured.

test_that("download_control happy path (live API)", {
  skip_if_no_live_api()
  local_db()
  setup_keywords()

  out <- capture_messages(
    download_control(control = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )

  n <- nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L & gt.env$dt_control$location != "world", ])
  expect_equal(n, 1800)
})

test_that("re-download control skips already-downloaded locations (live API)", {
  skip_if_no_live_api()
  local_db()
  setup_keywords()

  suppressMessages(download_control(control = 1, locations = location_set))

  out <- capture_messages(
    download_control(control = 1, locations = location_set[[1]])
  )

  expect_match(out, "No new locations to download \\| control: 1", all = FALSE)
})

test_that("download_control_global writes world aggregate (live API)", {
  skip_if_no_live_api()
  local_db()
  setup_keywords()

  expect_message(
    download_control_global(control = 1),
    "Downloaded control data \\| control: 1 \\| location: world \\[1/1\\]"
  )

  n <- nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L & gt.env$dt_control$location == "world", ])
  expect_equal(n, 600)
})

test_that("download_object happy path (live API)", {
  skip_if_no_live_api()
  local_db()
  setup_keywords()

  suppressMessages(download_control(control = 1, locations = location_set))

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )

  n <- nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L & gt.env$dt_object$location != "world", ])
  expect_equal(n, 1800)
})

test_that("re-download object skips already-downloaded (batch_c, batch_o, location) (live API)", {
  skip_if_no_live_api()
  local_db()
  setup_keywords()

  suppressMessages({
    download_control(control = 1, locations = location_set)
    download_object(object = 1, control = 1, locations = location_set)
  })

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set[[1]])
  )

  expect_match(
    out,
    "No new locations to download \\| object: 1 \\| control: 1",
    all = FALSE
  )
})

test_that("download_object_global writes world aggregate (live API)", {
  skip_if_no_live_api()
  local_db()
  setup_keywords()

  suppressMessages(download_control_global(control = 1))

  expect_message(
    download_object_global(object = 1, control = 1),
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: world \\[1/1\\]"
  )

  n <- nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L & gt.env$dt_object$location == "world", ])
  expect_equal(n, 600)
})

# ── Python Research API live tests (requires .env with GOOGLE_API_KEY + CONDA_ENV) ──
#
# These tests exercise the Google Trends Research API via the Python backend.
# They are skipped on CRAN, when offline, or when a .env file with
# GOOGLE_API_KEY and CONDA_ENV is not found in the package root.
#
# The .env file must live in the package root directory and follow the format:
#   GOOGLE_API_KEY=your_key_here
#   CONDA_ENV=/path/to/conda/env
# (.setup_python_api() and .parse_env_file() live in helper-skips.R.)

test_that("download_control happy path (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  out <- capture_messages(
    download_control(control = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded control data \\| control: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )

  n <- nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L & gt.env$dt_control$location != "world", ])
  expect_equal(n, 1800)
})

test_that("re-download control skips already-downloaded locations (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  suppressMessages(download_control(control = 1, locations = location_set))

  out <- capture_messages(
    download_control(control = 1, locations = location_set[[1]])
  )

  expect_match(out, "No new locations to download \\| control: 1", all = FALSE)
})

test_that("download_control_global writes world aggregate (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  expect_message(
    download_control_global(control = 1),
    "Downloaded control data \\| control: 1 \\| location: world \\[1/1\\]"
  )

  n <- nrow(gt.env$dt_control[gt.env$dt_control$batch == 1L & gt.env$dt_control$location == "world", ])
  expect_equal(n, 600)
})

test_that("download_object happy path (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  suppressMessages(download_control(control = 1, locations = location_set))

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )

  n <- nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L & gt.env$dt_object$location != "world", ])
  expect_equal(n, 1800)
})

test_that("re-download object skips already-downloaded (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  suppressMessages({
    download_control(control = 1, locations = location_set)
    download_object(object = 1, control = 1, locations = location_set)
  })

  out <- capture_messages(
    download_object(object = 1, control = 1, locations = location_set[[1]])
  )

  expect_match(
    out,
    "No new locations to download \\| object: 1 \\| control: 1",
    all = FALSE
  )
})

test_that("download_object_global writes world aggregate (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  suppressMessages(download_control_global(control = 1))

  expect_message(
    download_object_global(object = 1, control = 1),
    "Downloaded object data \\| object: 1 \\| control: 1 \\| location: world \\[1/1\\]"
  )

  n <- nrow(gt.env$dt_object[gt.env$dt_object$batch_o == 1L & gt.env$dt_object$location == "world", ])
  expect_equal(n, 600)
})

test_that("download_region happy path (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  out <- capture_messages(
    download_region(object = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded region data \\| object: 1 \\| location: US \\[1/3\\]",
    all = FALSE
  )

  dt <- gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ]
  expect_gt(nrow(dt), 0)
  expect_true(all(
    c("term", "location", "start_date", "end_date", "region_code",
      "region_name", "hits", "batch_o") %in% names(dt)
  ))
  expect_true(all(dt$location %in% location_set))
  expect_type(dt$hits, "double")
})

test_that("re-download region skips already-downloaded locations (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  suppressMessages(download_region(object = 1, locations = location_set))

  out <- capture_messages(
    download_region(object = 1, locations = location_set[[1]])
  )

  expect_match(out, "No new locations to download \\| object: 1", all = FALSE)
})

test_that("download_region_global writes world aggregate (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  expect_message(
    download_region_global(object = 1),
    "Downloaded region data \\| object: 1 \\| location: world \\[1/1\\]"
  )

  dt <- gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ]
  expect_gt(nrow(dt), 0)
  expect_true(all(dt$location == "world"))
})

test_that("download_topics happy path (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  out <- capture_messages(
    download_topics(object = 1, locations = location_set)
  )

  expect_match(
    out,
    "Downloaded related data \\| object: 1 \\| location: US \\| topic: TRUE \\| rising: FALSE \\[1/3\\]",
    all = FALSE
  )

  dt <- gt.env$dt_related[gt.env$dt_related$batch_o == 1L & gt.env$dt_related$topic == 1L & gt.env$dt_related$rising == 0L, ]
  expect_gt(nrow(dt), 0)
  expect_true(all(
    c("term", "topic", "rising", "location", "start_date", "end_date",
      "related_term", "hits", "batch_o") %in% names(dt)
  ))
  expect_true(all(dt$location %in% location_set))
  expect_type(dt$related_term, "character")
})

test_that("re-download topics skips already-downloaded locations (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  suppressMessages(download_topics(object = 1, locations = location_set))

  out <- capture_messages(
    download_topics(object = 1, locations = location_set[[1]])
  )

  expect_match(
    out,
    "No new locations to download \\| object: 1 \\| topic: TRUE \\| rising: FALSE",
    all = FALSE
  )
})

test_that("download_topics_global writes world aggregate (Python Research API)", {
  .setup_python_api()
  local_db()
  setup_keywords()

  expect_message(
    download_topics_global(object = 1),
    "Downloaded related data \\| object: 1 \\| location: world \\| topic: TRUE \\| rising: FALSE \\[1/1\\]"
  )

  dt <- gt.env$dt_related[gt.env$dt_related$batch_o == 1L & gt.env$dt_related$topic == 1L & gt.env$dt_related$rising == 0L, ]
  expect_gt(nrow(dt), 0)
  expect_true(all(dt$location == "world"))
})
