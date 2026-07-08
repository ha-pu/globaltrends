# Skip helpers shared across test files.

# Live gtrendsR tests hit the real (unofficial) Google Trends endpoint. They
# are opt-in: set GLOBALTRENDS_LIVE_TESTS=1 locally to run them. Without the
# opt-in they would run on every GitHub Actions job (the runner is online and
# not CRAN), making CI slow and flaky through rate limits.
skip_if_no_live_api <- function() {
  skip_on_cran()
  skip_if_offline()
  skip_if(
    !nzchar(Sys.getenv("GLOBALTRENDS_LIVE_TESTS")),
    "live API tests disabled (set GLOBALTRENDS_LIVE_TESTS=1 to enable)"
  )
}

# Python Research API live tests require a .env file in the package root:
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

# Skips unless a usable .env is present, then initializes the Python Research
# API backend and schedules py_setup to be reset when the test exits.
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
