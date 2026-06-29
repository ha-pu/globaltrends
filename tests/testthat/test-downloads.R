# Tests for download_control(), download_object(), and their *_global() wrappers.
#
# Structure:
#   Guard conditions     — purely offline; error/skip paths that fire before any
#                          API call reaches .get_trend().
#   Happy path (mocked)  — offline-safe; .get_trend() is stubbed with
#                          local_mocked_bindings() so these run without internet
#                          access and without Google Trends credentials.
#   Input validation     — purely offline; wrong argument types and values.
#   Live network tests   — guarded with skip_if_offline() + skip_on_cran();
#                          hit the real API for true end-to-end verification.
#
# The local_db() helper (from helper-db.R) provides fully isolated database
# state for every test. Cleanup is handled automatically via withr::defer().

source("../test_functions.r")
Sys.setenv("LANGUAGE" = "EN")

location_set <- c("US", "CN", "JP")

# ── Shared helpers ────────────────────────────────────────────────────────────

# Registers keyword batches that match the structure of the built-in example
# data (5 control keywords, 4 object keywords, 2010-01 to 2019-12).
setup_keywords <- function() {
  suppressMessages({
    add_control_keyword(
      keyword    = c("gmail", "map", "translate", "wikipedia", "youtube"),
      start_date = "2010-01",
      end_date   = "2019-12"
    )
    add_object_keyword(
      keyword    = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
      start_date = "2010-01",
      end_date   = "2019-12"
    )
  })
}

# Synthetic replacement for .get_trend(). Returns one row per (keyword, month)
# combination with hits = 50, satisfying the positive-signal check inside
# download_object(). Treats location = NULL or "" as the worldwide aggregate.
make_trend_data <- function(location = NULL, term, start_date, end_date) {
  dates <- seq(
    as.Date(paste0(start_date, "-01")),
    as.Date(paste0(end_date, "-01")),
    by = "month"
  )
  loc_out <- if (is.null(location) || identical(location, "")) "world" else location
  data.frame(
    location         = loc_out,
    keyword          = rep(term, each = length(dates)),
    date             = rep(dates, times = length(term)),
    hits             = 50,
    stringsAsFactors = FALSE
  )
}

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

# ── Live network integration (skipped when offline) ───────────────────────────
#
# These tests exercise the real gtrendsR / Research API backend. They are
# skipped on CRAN and whenever there is no internet connection. Run them
# locally after confirming credentials are configured.

test_that("download_control happy path (live API)", {
  skip_on_cran()
  skip_if_offline()
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
  skip_on_cran()
  skip_if_offline()
  local_db()
  setup_keywords()

  suppressMessages(download_control(control = 1, locations = location_set))

  out <- capture_messages(
    download_control(control = 1, locations = location_set[[1]])
  )

  expect_match(out, "No new locations to download \\| control: 1", all = FALSE)
})

test_that("download_control_global writes world aggregate (live API)", {
  skip_on_cran()
  skip_if_offline()
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
  skip_on_cran()
  skip_if_offline()
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
  skip_on_cran()
  skip_if_offline()
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
  skip_on_cran()
  skip_if_offline()
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

.parse_env_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines)) & !grepl("^\\s*#", lines)]
  pairs <- strsplit(lines, "=", fixed = TRUE)
  pairs <- pairs[lengths(pairs) >= 2]
  setNames(
    vapply(pairs, function(x) trimws(paste(x[-1], collapse = "=")), character(1)),
    vapply(pairs, function(x) trimws(x[[1]]), character(1))
  )
}

.setup_python_api <- function(env = parent.frame()) {
  skip_on_cran()
  skip_if_offline()

  # Accept .env whether the working directory is the package root or tests/testthat/
  candidates <- c(".env", file.path("..", "..", ".env"))
  env_file <- Find(file.exists, candidates)
  skip_if(is.null(env_file), ".env not found in package root — skipping Python Research API tests")

  env_vars <- .parse_env_file(env_file)
  api_key <- env_vars[["GOOGLE_API_KEY"]]
  conda_env <- env_vars[["CONDA_ENV"]]

  skip_if(is.na(api_key), "GOOGLE_API_KEY not found in .env")
  skip_if(!nzchar(api_key), "GOOGLE_API_KEY is empty in .env")
  skip_if(is.na(conda_env), "CONDA_ENV not found in .env")
  skip_if(!nzchar(conda_env), "CONDA_ENV is empty in .env")

  suppressMessages(initialize_python(api_key = api_key, conda_env = conda_env))
  withr::defer(gt.env$py_setup <- FALSE, envir = env)
}

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

  n <- nrow(gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ])
  expect_gt(n, 0)
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

  n <- nrow(gt.env$dt_region[gt.env$dt_region$batch_o == 1L, ])
  expect_gt(n, 0)
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

  n <- nrow(gt.env$dt_related[gt.env$dt_related$batch_o == 1L & gt.env$dt_related$topic == 1L & gt.env$dt_related$rising == 0L, ])
  expect_gt(n, 0)
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

  n <- nrow(gt.env$dt_related[gt.env$dt_related$batch_o == 1L & gt.env$dt_related$topic == 1L & gt.env$dt_related$rising == 0L, ])
  expect_gt(n, 0)
})
