# Shared gt.env fixtures and synthetic-data builders. Auto-loaded by testthat.
#
# NOTE: all reusable test helpers live in helper-*.R files (never at the top
# level of a test-*.R file) so that devtools::test(shuffle = TRUE) — which
# shuffles ALL top-level expressions in a test file — cannot run a test
# before the helper it needs is defined.

# The location trio used throughout the download/computation tests; matches
# the locations available in the example_* datasets.
location_set <- c("US", "CN", "JP")

# Replaces the .wait() seam with a recorder so retry/backoff logic runs
# instantly. Returns an environment whose $log accumulates the requested wait
# durations.
local_recorded_waits <- function(env = parent.frame()) {
  waits <- new.env(parent = emptyenv())
  waits$log <- numeric()
  testthat::local_mocked_bindings(
    .wait = function(seconds) {
      waits$log <- c(waits$log, seconds)
      invisible(NULL)
    },
    .package = "globaltrends",
    .env = env
  )
  waits
}

# A gtrends() stand-in that fails `fail_times` times with `msg`, then returns
# a minimal successful result.
make_flaky_gtrends <- function(fail_times, msg) {
  calls <- 0L
  function(...) {
    calls <<- calls + 1L
    if (calls <= fail_times) {
      stop(msg)
    }
    list(interest_over_time = data.frame(
      geo              = "US",
      keyword          = "gmail",
      date             = as.Date("2020-01-01"),
      hits             = "50",
      stringsAsFactors = FALSE
    ))
  }
}

# Installs a fake gt.env$query_region and restores it on exit.
local_query_region <- function(fun, env = parent.frame()) {
  saved <- gt.env$query_region
  withr::defer(gt.env$query_region <- saved, envir = env)
  gt.env$query_region <- fun
}

# Installs a fake gt.env$query_terms and restores it on exit.
local_query_terms <- function(fun, env = parent.frame()) {
  saved <- gt.env$query_terms
  withr::defer(gt.env$query_terms <- saved, envir = env)
  gt.env$query_terms <- fun
}

# Saves and restores the gt.env fields initialize_python() writes.
local_python_init_state <- function(env = parent.frame()) {
  saved_py_setup <- gt.env$py_setup
  saved_api_key <- gt.env$api_key
  withr::defer(
    {
      gt.env$py_setup <- saved_py_setup
      gt.env$api_key <- saved_api_key
    },
    envir = env
  )
  invisible(NULL)
}

# Saves gt.env$py_setup and restores it on test exit.
local_py_setup_state <- function(env = parent.frame()) {
  saved_py_setup <- gt.env$py_setup
  withr::defer(gt.env$py_setup <- saved_py_setup, envir = env)
  invisible(NULL)
}

# Resets api_calls to 0 and api_calls_date to today, restoring originals on
# test exit.
local_counter_state <- function(env = parent.frame()) {
  saved_calls <- gt.env$api_calls
  saved_date <- gt.env$api_calls_date
  gt.env$api_calls <- 0L
  gt.env$api_calls_date <- Sys.Date()
  withr::defer(
    {
      gt.env$api_calls <- saved_calls
      gt.env$api_calls_date <- saved_date
    },
    envir = env
  )
  invisible(NULL)
}

# Activates the Python Research API backend without a real Python environment
# by injecting a fake query_trend function. Restores all touched gt.env fields
# on test exit.
local_py_state <- function(env = parent.frame()) {
  saved_py_setup <- gt.env$py_setup
  saved_query_trend <- gt.env$query_trend
  saved_query_wait <- gt.env$query_wait
  saved_api_key <- gt.env$api_key
  withr::defer(
    {
      gt.env$py_setup <- saved_py_setup
      gt.env$query_trend <- saved_query_trend
      gt.env$query_wait <- saved_query_wait
      gt.env$api_key <- saved_api_key
    },
    envir = env
  )

  gt.env$py_setup <- TRUE
  gt.env$query_wait <- 0
  gt.env$api_key <- "dummy"
  gt.env$query_trend <- function(terms, start_date, end_date, geo, api_key) {
    list(lines = lapply(terms, function(t) {
      list(term = t, points = list(list(date = "2020-01-01", value = 50L)))
    }))
  }
  invisible(NULL)
}

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
