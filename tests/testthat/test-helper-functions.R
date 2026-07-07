# Unit tests for the internal download/retry helpers in R/helper_functions.r:
# .retry_gtrends(), .retry_py_call(), .get_trend(), .get_region(),
# .get_related(), .get_full(), .test_empty(), and the persistence checkpoints
# in .increment_api_counter()/.increment_score_counter().
#
# All tests run offline. Network seams are stubbed:
#   - gtrends() (imported from gtrendsR) via local_mocked_bindings()
#   - the Python backend via fake gt.env$query_* functions (local_py_state()
#     from helper-fixtures.R)
#   - all sleeps via the .wait() seam, recorded by local_recorded_waits()
#     (defined with the other shared fixtures in helper-fixtures.R)

# ── .retry_gtrends() ──────────────────────────────────────────────────────────

test_that(".retry_gtrends returns the result on first success without waiting", {
  waits <- local_recorded_waits()
  local_mocked_bindings(
    gtrends = make_flaky_gtrends(0, "unused"),
    .package = "globaltrends"
  )

  out <- globaltrends:::.retry_gtrends(keyword = "gmail")

  expect_type(out, "list")
  expect_s3_class(out$interest_over_time, "data.frame")
  expect_length(waits$log, 0)
})

test_that(".retry_gtrends retries an HTTP 500 quickly (1s) and then succeeds", {
  waits <- local_recorded_waits()
  local_mocked_bindings(
    gtrends = make_flaky_gtrends(2, "Returned status code:500"),
    .package = "globaltrends"
  )

  msgs <- capture_messages(
    out <- globaltrends:::.retry_gtrends(keyword = "gmail")
  )

  expect_s3_class(out$interest_over_time, "data.frame")
  expect_match(msgs, "globaltrends retrying download in 1s \\(HTTP 500\\)\\.", all = TRUE)
  expect_equal(waits$log, c(1, 1))
})

test_that(".retry_gtrends waits 60s for non-500 errors before retrying", {
  waits <- local_recorded_waits()
  local_mocked_bindings(
    gtrends = make_flaky_gtrends(1, "Returned status code:429"),
    .package = "globaltrends"
  )

  msgs <- capture_messages(
    out <- globaltrends:::.retry_gtrends(keyword = "gmail")
  )

  expect_s3_class(out$interest_over_time, "data.frame")
  expect_match(msgs, "globaltrends retrying download in 60s\\.", all = FALSE)
  expect_equal(waits$log, 60)
})

test_that(".retry_gtrends errors with the last message after max_tries attempts", {
  waits <- local_recorded_waits()
  local_mocked_bindings(
    gtrends = make_flaky_gtrends(Inf, "Returned status code:500"),
    .package = "globaltrends"
  )

  suppressMessages(expect_error(
    globaltrends:::.retry_gtrends(keyword = "gmail", max_tries = 3),
    "Download failed after 3 attempts.\nLast error: Returned status code:500",
    fixed = TRUE
  ))
  # max_tries = 3 means 3 attempts, so 2 waits between them.
  expect_equal(waits$log, c(1, 1))
})

# ── .retry_py_call() ──────────────────────────────────────────────────────────

test_that(".retry_py_call returns the call's value on first success", {
  waits <- local_recorded_waits()

  out <- globaltrends:::.retry_py_call(function() "ok")

  expect_equal(out, "ok")
  expect_length(waits$log, 0)
})

test_that(".retry_py_call retries transient errors with doubling backoff", {
  waits <- local_recorded_waits()
  calls <- 0L
  flaky <- function() {
    calls <<- calls + 1L
    if (calls <= 2L) {
      stop("HTTP 503 Service Unavailable")
    }
    "recovered"
  }

  msgs <- capture_messages(
    out <- globaltrends:::.retry_py_call(flaky, max_tries = 5, wait = 5)
  )

  expect_equal(out, "recovered")
  expect_equal(calls, 3L)
  expect_equal(waits$log, c(5, 10))
  expect_match(msgs, "Transient Google Trends API error \\(attempt 1/5\\)", all = FALSE)
  expect_match(msgs, "Transient Google Trends API error \\(attempt 2/5\\)", all = FALSE)
})

test_that(".retry_py_call rethrows non-transient errors immediately", {
  waits <- local_recorded_waits()

  expect_error(
    globaltrends:::.retry_py_call(function() stop("429 rateLimitExceeded")),
    "429 rateLimitExceeded"
  )
  expect_length(waits$log, 0)
})

test_that(".retry_py_call gives up after max_tries transient failures", {
  waits <- local_recorded_waits()

  suppressMessages(expect_error(
    globaltrends:::.retry_py_call(
      function() stop("502 Bad Gateway"),
      max_tries = 3,
      wait = 2
    ),
    "502 Bad Gateway"
  ))
  expect_equal(waits$log, c(2, 4))
})

# ── .get_trend() — Research API backend ───────────────────────────────────────

test_that(".get_trend returns tidy rows from the Research API backend", {
  local_counter_state()
  local_py_state()

  out <- globaltrends:::.get_trend(
    location   = "US",
    term       = c("gmail", "wikipedia"),
    start_date = "2020-01",
    end_date   = "2020-01"
  )

  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("location", "keyword", "date", "hits"))
  expect_setequal(out$keyword, c("gmail", "wikipedia"))
  expect_true(all(out$location == "US"))
  expect_equal(unique(out$date), as.Date("2020-01-01"))
  expect_true(all(out$hits == 50))
})

test_that(".get_trend maps NULL location to the world aggregate", {
  local_counter_state()
  local_py_state()

  out <- globaltrends:::.get_trend(
    location   = NULL,
    term       = "gmail",
    start_date = "2020-01",
    end_date   = "2020-01"
  )

  expect_true(all(out$location == "world"))
})

test_that(".get_trend stops with a quota error and persists the database on HTTP 429", {
  local_db()
  local_counter_state()
  local_py_state()
  gt.env$query_trend <- function(...) stop("HTTP 429 Quota exceeded")

  expect_error(
    globaltrends:::.get_trend(location = "US", term = "gmail"),
    "Google Trends API daily quota exceeded"
  )

  # The quota handler called disconnect_db(): handles are cleared and the
  # store was persisted.
  expect_null(gt.env$dt_control)
  expect_true(file.exists(file.path("db", "globaltrends.rds")))

  # Reopen so local_db()'s deferred disconnect_db() has a session to close.
  suppressMessages(start_db())
})

test_that(".get_trend skips with a message and returns NULL on HTTP 400", {
  local_counter_state()
  local_py_state()
  gt.env$query_trend <- function(...) stop("HTTP 400 badRequest")

  msgs <- capture_messages(
    out <- globaltrends:::.get_trend(location = "US", term = "gmail")
  )

  expect_null(out)
  expect_match(
    msgs,
    "Skipping: API returned HTTP 400 \\(invalid argument\\) for term=gmail geo=US",
    all = FALSE
  )
  expect_equal(gt.env$api_calls, 0L)
})

test_that(".get_trend skips with a message and returns NULL on connection timeout", {
  local_counter_state()
  local_py_state()
  gt.env$query_trend <- function(...) stop("connection timed out")

  msgs <- capture_messages(
    out <- globaltrends:::.get_trend(location = "US", term = "gmail")
  )

  expect_null(out)
  expect_match(msgs, "Skipping: connection timeout for term=gmail geo=US", all = FALSE)
})

test_that(".get_trend rethrows unrecognized Research API errors", {
  local_counter_state()
  local_py_state()
  gt.env$query_trend <- function(...) stop("some unexpected failure")

  expect_error(
    globaltrends:::.get_trend(location = "US", term = "gmail"),
    "some unexpected failure"
  )
})

test_that(".get_trend validates its arguments", {
  local_py_state()

  expect_error(
    globaltrends:::.get_trend(term = 1),
    "`term` must be of type character"
  )
  expect_error(
    globaltrends:::.get_trend(term = "gmail", start_date = c("2020-01", "2020-02")),
    "`start_date` must have length <= 1"
  )
  expect_error(
    globaltrends:::.get_trend(term = "gmail", start_date = 1),
    "`start_date` must be of type character"
  )
})

# ── .get_trend() — gtrendsR fallback backend ──────────────────────────────────

test_that(".get_trend uses gtrendsR when py_setup is FALSE and tidies its output", {
  local_counter_state()
  local_py_setup_state()
  gt.env$py_setup <- FALSE
  waits <- local_recorded_waits()
  local_mocked_bindings(
    gtrends = function(keyword, geo, time, onlyInterest) {
      list(interest_over_time = data.frame(
        geo              = "US",
        keyword          = c("gmail", "gmail"),
        date             = as.Date(c("2020-01-01", "2020-02-01")),
        hits             = c("<1", "50"),
        stringsAsFactors = FALSE
      ))
    },
    .package = "globaltrends"
  )

  out <- globaltrends:::.get_trend(
    location   = "US",
    term       = "gmail",
    start_date = "2020-01",
    end_date   = "2020-02"
  )

  expect_named(out, c("location", "keyword", "date", "hits"))
  # "<1" is converted to 0.1 and hits become numeric.
  expect_equal(out$hits, c(0.1, 50))
  expect_s3_class(out$date, "Date")
  # The jitter pause stays within its documented 5-10s range.
  expect_length(waits$log, 1)
  expect_gte(waits$log, 5)
  expect_lte(waits$log, 10)
  # gtrendsR calls must not consume Research API quota.
  expect_equal(gt.env$api_calls, 0L)
})

test_that(".get_trend returns NULL when gtrendsR yields no interest_over_time", {
  local_py_setup_state()
  gt.env$py_setup <- FALSE
  local_recorded_waits()
  local_mocked_bindings(
    gtrends = function(...) list(interest_over_time = NULL),
    .package = "globaltrends"
  )

  expect_null(globaltrends:::.get_trend(location = "US", term = "gmail"))
})

# ── .get_region() ─────────────────────────────────────────────────────────────

test_that(".get_region returns one tidy row per region", {
  local_counter_state()
  local_py_state()
  local_query_region(function(terms, start_date, end_date, geo, api_key) {
    list(regions = list(
      list(regionCode = "US-CA", regionName = "California", value = 80),
      list(regionCode = "US-NY", regionName = "New York", value = 60)
    ))
  })

  out <- globaltrends:::.get_region(
    location   = "US",
    term       = "gmail",
    start_date = "2020-01",
    end_date   = "2020-12"
  )

  expect_named(
    out,
    c("term", "location", "start_date", "end_date", "region_code", "region_name", "hits")
  )
  expect_equal(out$region_code, c("US-CA", "US-NY"))
  expect_equal(out$hits, c(80, 60))
  expect_true(all(out$term == "gmail"))
  expect_true(all(out$location == "US"))
  expect_equal(unique(out$start_date), as.Date("2020-01-01"))
  expect_equal(gt.env$api_calls, 1L)
})

test_that(".get_region maps NULL location to the world aggregate", {
  local_counter_state()
  local_py_state()
  local_query_region(function(...) {
    list(regions = list(list(regionCode = "ES-CT", regionName = "Catalonia", value = 100)))
  })

  out <- globaltrends:::.get_region(location = NULL, term = "gmail")

  expect_true(all(out$location == "world"))
})

test_that(".get_region stops with a quota error on HTTP 429", {
  local_db()
  local_py_state()
  local_query_region(function(...) stop("rateLimitExceeded"))

  expect_error(
    globaltrends:::.get_region(location = "US", term = "gmail"),
    "Google Trends API daily quota exceeded"
  )

  expect_null(gt.env$dt_control)
  suppressMessages(start_db())
})

test_that(".get_region returns NULL when the Research API backend is inactive", {
  local_py_setup_state()
  gt.env$py_setup <- FALSE

  expect_null(globaltrends:::.get_region(location = "US", term = "gmail"))
})

test_that(".get_region rejects a term vector of length > 1", {
  local_py_state()

  expect_error(
    globaltrends:::.get_region(term = c("a", "b")),
    "`term` must have length <= 1"
  )
})

# ── .get_related() ────────────────────────────────────────────────────────────

test_that(".get_related returns one tidy row per related term", {
  local_counter_state()
  local_py_state()
  local_query_terms(function(terms, start_date, end_date, geo, api_key, topic, rising) {
    list(item = list(
      list(title = "google mail", value = 100),
      list(title = "email", value = 55)
    ))
  })

  out <- globaltrends:::.get_related(
    location   = "US",
    term       = "gmail",
    start_date = "2020-01",
    end_date   = "2020-12",
    topic      = TRUE,
    rising     = FALSE
  )

  expect_setequal(
    names(out),
    c("related_term", "hits", "term", "topic", "rising", "location", "start_date", "end_date")
  )
  expect_equal(out$related_term, c("google mail", "email"))
  expect_equal(out$hits, c(100, 55))
  expect_true(all(out$topic))
  expect_false(any(out$rising))
  expect_true(all(out$location == "US"))
  expect_equal(gt.env$api_calls, 1L)
})

test_that(".get_related skips with a message and returns NULL on HTTP 400", {
  local_counter_state()
  local_py_state()
  local_query_terms(function(...) stop("HTTP 400 invalid argument"))

  msgs <- capture_messages(
    out <- globaltrends:::.get_related(
      location = "US", term = "gmail", topic = TRUE, rising = FALSE
    )
  )

  expect_null(out)
  expect_match(msgs, "Skipping: API returned HTTP 400", all = FALSE)
})

test_that(".get_related skips with a message and returns NULL on timeout", {
  local_counter_state()
  local_py_state()
  local_query_terms(function(...) stop("TimeoutError"))

  msgs <- capture_messages(
    out <- globaltrends:::.get_related(
      location = "US", term = "gmail", topic = FALSE, rising = TRUE
    )
  )

  expect_null(out)
  expect_match(msgs, "Skipping: connection timeout", all = FALSE)
})

test_that(".get_related stops with a quota error on HTTP 429", {
  local_db()
  local_py_state()
  local_query_terms(function(...) stop("Quota exceeded"))

  expect_error(
    globaltrends:::.get_related(
      location = "US", term = "gmail", topic = TRUE, rising = FALSE
    ),
    "Google Trends API daily quota exceeded"
  )

  expect_null(gt.env$dt_control)
  suppressMessages(start_db())
})

test_that(".get_related requires logical topic and rising flags", {
  local_py_state()

  expect_error(
    globaltrends:::.get_related(term = "gmail", topic = "yes", rising = FALSE),
    "`topic` must be of type logical"
  )
  expect_error(
    globaltrends:::.get_related(term = "gmail", topic = TRUE, rising = 1),
    "`rising` must be of type logical"
  )
})

# ── Persistence checkpoints ───────────────────────────────────────────────────

test_that(".increment_api_counter persists and reloads the database at 1000 calls", {
  local_db()
  local_counter_state()

  # Marker row that must survive the checkpoint round-trip to disk.
  seed_table("dt_keywords", data.frame(
    type = "control", batch = 1L, keyword = "checkpoint_marker"
  ))
  gt.env$api_calls <- 999L

  msgs <- capture_messages(globaltrends:::.increment_api_counter())

  expect_match(
    msgs,
    "Persisting in-memory data to local file after 1000 API calls.",
    fixed = TRUE,
    all = FALSE
  )
  expect_equal(gt.env$api_calls, 1000L)
  # Session was reopened after the checkpoint ...
  expect_false(is.null(gt.env$dt_control))
  # ... and the marker actually reached the file.
  saved <- readRDS(file.path("db", "globaltrends.rds"))
  expect_true("checkpoint_marker" %in% saved$batch_keywords$keyword)
})

test_that(".increment_api_counter does not checkpoint without an active session", {
  local_counter_state()
  saved_control <- gt.env$dt_control
  gt.env$dt_control <- NULL
  withr::defer(gt.env$dt_control <- saved_control)
  gt.env$api_calls <- 999L

  expect_no_message(globaltrends:::.increment_api_counter())
  expect_equal(gt.env$api_calls, 1000L)
})

test_that(".increment_score_counter initializes a missing counter and persists at 1000", {
  local_db()
  saved_calls <- gt.env$score_calls
  withr::defer(gt.env$score_calls <- saved_calls)

  gt.env$score_calls <- NULL
  globaltrends:::.increment_score_counter()
  expect_equal(gt.env$score_calls, 1L)

  gt.env$score_calls <- 999L
  msgs <- capture_messages(globaltrends:::.increment_score_counter())

  expect_match(
    msgs,
    "Persisting in-memory data to local file after 1000 computed locations.",
    fixed = TRUE,
    all = FALSE
  )
  expect_equal(gt.env$score_calls, 1000L)
  expect_false(is.null(gt.env$dt_control))
})

# ── .get_full() ───────────────────────────────────────────────────────────────

test_that(".get_full returns only the locations of the requested control batch", {
  local_db()
  seed_table("dt_control", data.frame(
    location = c("US", "CN", "JP"),
    keyword  = "gmail",
    date     = 1,
    hits     = 50,
    batch    = c(1L, 1L, 2L)
  ))

  expect_setequal(
    globaltrends:::.get_full("data_control", in_batch_c = 1),
    c("US", "CN")
  )
  expect_equal(globaltrends:::.get_full("data_control", in_batch_c = 2), "JP")
  expect_identical(
    globaltrends:::.get_full("data_control", in_batch_c = 99),
    character(0)
  )
})

test_that(".get_full filters data_object and data_score by both batch ids", {
  local_db()
  seed_table("dt_object", data.frame(
    location = c("US", "CN", "JP"),
    keyword  = "kw",
    date     = 1,
    hits     = 50,
    batch_c  = c(1L, 1L, 2L),
    batch_o  = c(1L, 2L, 1L)
  ))
  seed_table("dt_score", data.frame(
    location = c("US", "CN"),
    keyword  = "kw",
    date     = 1,
    score    = 0.5,
    batch_c  = 1L,
    batch_o  = c(1L, 2L)
  ))

  expect_equal(
    globaltrends:::.get_full("data_object", in_batch_c = 1, in_batch_o = 1),
    "US"
  )
  expect_equal(
    globaltrends:::.get_full("data_score", in_batch_c = 1, in_batch_o = 2),
    "CN"
  )
})

test_that(".get_full filters data_related by batch, topic, and rising", {
  local_db()
  seed_table("dt_related", data.frame(
    term         = "kw",
    topic        = c(1L, 1L, 0L),
    rising       = c(0L, 1L, 0L),
    location     = c("US", "CN", "JP"),
    start_date   = 1,
    end_date     = 2,
    related_term = "rel",
    hits         = 10,
    batch_o      = 1L
  ))

  expect_equal(
    globaltrends:::.get_full(
      "data_related",
      in_batch_o = 1, in_topic = TRUE, in_rising = FALSE
    ),
    "US"
  )
  expect_equal(
    globaltrends:::.get_full(
      "data_related",
      in_batch_o = 1, in_topic = FALSE, in_rising = FALSE
    ),
    "JP"
  )
})

test_that(".get_full returns character(0) for empty tables", {
  local_db()

  expect_identical(
    globaltrends:::.get_full("data_object", in_batch_c = 1, in_batch_o = 1),
    character(0)
  )
  expect_identical(
    globaltrends:::.get_full("data_region", in_batch_o = 1),
    character(0)
  )
})

test_that(".get_full errors when required identifiers are missing", {
  local_db()

  expect_error(
    globaltrends:::.get_full("data_control"),
    "`batch_c` must be provided for table = 'data_control'."
  )
  expect_error(
    globaltrends:::.get_full("data_object", in_batch_c = 1),
    "`batch_o` must be provided for table = 'data_object'."
  )
  expect_error(
    globaltrends:::.get_full("data_related", in_batch_o = 1),
    "`topic` must be provided for table = 'data_related'."
  )
  expect_error(
    globaltrends:::.get_full("data_related", in_batch_o = 1, in_topic = TRUE),
    "`rising` must be provided for table = 'data_related'."
  )
})

test_that(".get_full validates its table argument", {
  local_db()

  expect_error(
    globaltrends:::.get_full("nonexistent_table", in_batch_c = 1),
    "`table` must be one of 'data_control', 'data_object', 'data_score', 'data_region', or 'data_related'."
  )
  expect_error(
    globaltrends:::.get_full(1),
    "`table` must be of type character"
  )
})

# ── .test_empty() ─────────────────────────────────────────────────────────────

test_that(".test_empty detects existing and missing DOI rows", {
  local_db()
  seed_table("dt_doi", data.frame(
    keyword = "kw", date = 1, gini = 0.5, hhi = 0.5, entropy = -0.5,
    batch_c = 1L, batch_o = 1L, locations = "countries"
  ))

  expect_false(
    globaltrends:::.test_empty(batch_c = 1, batch_o = 1, locations = "countries")
  )
  expect_true(
    globaltrends:::.test_empty(batch_c = 1, batch_o = 2, locations = "countries")
  )
  expect_true(
    globaltrends:::.test_empty(batch_c = 1, batch_o = 1, locations = "us_states")
  )
})

test_that(".test_empty validates batch and locations arguments", {
  local_db()

  expect_error(
    globaltrends:::.test_empty(batch_c = 1.5, batch_o = 1, locations = "countries"),
    "Batch id must be an integer value."
  )
  expect_error(
    globaltrends:::.test_empty(batch_c = 1, batch_o = 1, locations = 1),
    "`locations` must be of type character"
  )
})
