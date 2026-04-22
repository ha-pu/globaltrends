#' @title Initialize Python backend for Google Trends Research API
#'
#' @description
#' Initializes the Python session required to download data via the Google
#' Trends *Research API* (not the public `gtrendsR::gtrends()` scraping route).
#' The function configures the Python interpreter (Conda or virtualenv),
#' stores the API key in `gt.env`, sources the package's Python helper code,
#' and marks the session as ready for API-based downloads.
#'
#' @details
#' **Prerequisites.** Before calling `initialize_python()`:
#' 1. Apply for Research API access and obtain an API key via Google's request
#'    form.
#' 2. Create a Python environment (Conda or virtualenv) with
#'    `google-api-python-client` installed.
#'
#' **Environment specification.** Exactly one of `conda_env` or `python_env`
#' must be supplied; providing neither or both is an error.
#'
#' **Effect on the download backend.** Once initialized, all download functions
#' ([download_control()], [download_object()], [download_region()],
#' [download_related()]) switch from the default `gtrendsR::gtrends()` scraping
#' route to the Research API.
#'
#' @param api_key Character scalar. API key obtained from Google.
#'
#' @param conda_env Optional character scalar. Name or path of a Conda
#'   environment (passed to [reticulate::use_condaenv()]). Supply either this
#'   or `python_env`, not both.
#'
#' @param python_env Optional character scalar. Path to a Python virtual
#'   environment (passed to [reticulate::use_virtualenv()]). Supply either this
#'   or `conda_env`, not both.
#'
#' @return
#' Invisibly returns `TRUE`. Called for its side effects: stores `api_key` in
#' `gt.env`, sources `python/query_gtrends.py`, and sets `gt.env$py_setup` to
#' `TRUE` to activate the Research API download backend.
#'
#' @seealso
#' [download_control()], [download_object()], [download_region()],
#' [download_related()] for the download functions that use the Research API
#' once initialized.
#' [reticulate::use_condaenv()] and [reticulate::use_virtualenv()] for Python
#' environment configuration.
#'
#' @examples
#' \dontrun{
#' # Conda environment
#' initialize_python(
#'   api_key   = "YOUR_API_KEY",
#'   conda_env = "/path/to/conda/env"
#' )
#'
#' # Virtual environment
#' initialize_python(
#'   api_key    = "YOUR_API_KEY",
#'   python_env = "/path/to/venv"
#' )
#' }
#'
#' @export
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python

initialize_python <- function(api_key, conda_env = NULL, python_env = NULL) {
  .check_length(api_key, 1)
  .check_input(api_key, "character")

  if (is.null(conda_env) && is.null(python_env)) {
    stop("Specify exactly one of `conda_env` or `python_env`.", call. = FALSE)
  }
  if (!is.null(conda_env) && !is.null(python_env)) {
    stop(
      "Specify only one of `conda_env` or `python_env`, not both.",
      call. = FALSE
    )
  }

  if (!is.null(conda_env)) {
    .check_length(conda_env, 1)
    .check_input(conda_env, "character")
  } else {
    .check_length(python_env, 1)
    .check_input(python_env, "character")
  }

  # Clearing RETICULATE_PYTHON avoids accidentally pinning a different interpreter.
  Sys.unsetenv("RETICULATE_PYTHON")

  if (!is.null(conda_env)) {
    use_condaenv(conda_env, required = TRUE)
  } else {
    use_virtualenv(python_env, required = TRUE)
  }

  gt.env$api_key <- api_key

  py_file <- system.file("python/query_gtrends.py", package = "globaltrends")
  if (identical(py_file, "")) {
    stop(
      "Could not find 'python/query_gtrends.py' in the installed package.",
      call. = FALSE
    )
  }

  source_python(file = py_file, envir = gt.env)

  gt.env$py_setup <- TRUE
  invisible(TRUE)
}
