# Tests for the Research API usage counter: get_api_usage() and
# .increment_api_counter().
#
# All tests are fully offline. They manipulate gt.env directly or stub internal
# Python-facing functions via withr::defer / local_mocked_bindings() so that no
# network calls or Python environment are required.

# ── get_api_usage() ───────────────────────────────────────────────────────────

test_that("get_api_usage() returns a named integer vector with the expected elements", {
  local_counter_state()

  out <- get_api_usage()

  expect_type(out, "integer")
  expect_named(out, c("calls", "remaining", "limit"))
})

test_that("get_api_usage() reports zero calls and full quota when no calls have been made", {
  local_counter_state()

  out <- get_api_usage()

  expect_equal(out[["calls"]], 0L)
  expect_equal(out[["remaining"]], 10000L)
  expect_equal(out[["limit"]], 10000L)
})

test_that("get_api_usage() reflects the current call count and remaining quota", {
  local_counter_state()
  gt.env$api_calls <- 250L

  out <- get_api_usage()

  expect_equal(out[["calls"]], 250L)
  expect_equal(out[["remaining"]], 9750L)
  expect_equal(out[["limit"]], 10000L)
})

test_that("get_api_usage() resets the counter when the stored date is before today", {
  local_counter_state()
  gt.env$api_calls <- 500L
  gt.env$api_calls_date <- Sys.Date() - 1L

  out <- get_api_usage()

  expect_equal(out[["calls"]], 0L)
  expect_equal(out[["remaining"]], 10000L)
  expect_equal(gt.env$api_calls_date, Sys.Date())
})

# ── .increment_api_counter() ──────────────────────────────────────────────────

test_that(".increment_api_counter() increments api_calls by 1", {
  local_counter_state()

  globaltrends:::.increment_api_counter()

  expect_equal(gt.env$api_calls, 1L)
})

test_that(".increment_api_counter() accumulates correctly across multiple calls", {
  local_counter_state()

  globaltrends:::.increment_api_counter()
  globaltrends:::.increment_api_counter()
  globaltrends:::.increment_api_counter()

  expect_equal(gt.env$api_calls, 3L)
})

test_that(".increment_api_counter() resets to 1 when the stored date is stale", {
  local_counter_state()
  gt.env$api_calls <- 100L
  gt.env$api_calls_date <- Sys.Date() - 1L

  globaltrends:::.increment_api_counter()

  expect_equal(gt.env$api_calls, 1L)
  expect_equal(gt.env$api_calls_date, Sys.Date())
})

# ── Integration: counter wiring in .get_trend() ───────────────────────────────

test_that("counter increments by 1 per .get_trend() call when py_setup is TRUE", {
  local_counter_state()
  local_py_state()

  globaltrends:::.get_trend(
    location   = "US",
    term       = c("gmail", "wikipedia"),
    start_date = "2020-01",
    end_date   = "2020-01"
  )

  expect_equal(gt.env$api_calls, 1L)
})

test_that("counter accumulates across repeated .get_trend() calls (one per location)", {
  local_counter_state()
  local_py_state()

  globaltrends:::.get_trend(location = "US", term = "gmail", start_date = "2020-01", end_date = "2020-01")
  globaltrends:::.get_trend(location = "CN", term = "gmail", start_date = "2020-01", end_date = "2020-01")
  globaltrends:::.get_trend(location = "JP", term = "gmail", start_date = "2020-01", end_date = "2020-01")

  expect_equal(gt.env$api_calls, 3L)
})

test_that("counter does not increment via .get_trend() when py_setup is FALSE", {
  local_counter_state()

  saved_py_setup <- gt.env$py_setup
  gt.env$py_setup <- FALSE
  withr::defer(gt.env$py_setup <- saved_py_setup)

  local_mocked_bindings(
    .retry_gtrends = function(...) {
      list(interest_over_time = data.frame(
        geo              = "US",
        keyword          = "gmail",
        date             = as.Date("2020-01-01"),
        hits             = 50,
        stringsAsFactors = FALSE
      ))
    },
    .package = "globaltrends"
  )

  globaltrends:::.get_trend(
    location   = "US",
    term       = "gmail",
    start_date = "2020-01",
    end_date   = "2020-01"
  )

  expect_equal(gt.env$api_calls, 0L)
})

test_that("get_api_usage() and .increment_api_counter() stay in sync", {
  local_counter_state()

  globaltrends:::.increment_api_counter()
  globaltrends:::.increment_api_counter()

  out <- get_api_usage()

  expect_equal(out[["calls"]], 2L)
  expect_equal(out[["remaining"]], 9998L)
})
