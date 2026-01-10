#' @title Initialize Python backend for Google Trends Research API
#'
#' @description
#' Initializes the Python session required to download data via the Google
#' Trends *Research API* (not the public `gtrendsR::gtrends()` scraping route).
#' The function configures the Python interpreter (Conda *or* virtualenv),
#' stores the API key in `gt.env`, sources the package's Python helper code,
#' and marks the session as ready for API-based downloads.
#'
#' @details
#' Prerequisites for using the Research API:
#' 1. Apply for access and obtain an API key via Google's request form.
#' 2. Create a Python environment (Conda or virtualenv) with the required
#'    dependencies installed (at minimum `google-api-python-client`).
#'
#' You must provide exactly one of `conda_env` or `python_env`.
#'
#' @param api_key Character scalar. API key obtained from Google.
#'
#' @param conda_env Optional character scalar. Name or path of a Conda
#'   environment to use (passed to [reticulate::use_condaenv()]).
#'
#' @param python_env Optional character scalar. Path to a Python virtual
#'   environment to use (passed to [reticulate::use_virtualenv()]).
#'
#' @return
#' Invisibly returns `TRUE` on success. Side effects:
#' \itemize{
#'   \item Stores `api_key` in `gt.env`.
#'   \item Sources the Python helper script `python/query_gtrends.py` into `gt.env`.
#'   \item Sets `gt.env$py_setup <- TRUE` to indicate API-mode is available.
#' }
#'
#' @examples
#' \dontrun{
#' initialize_python(
#'   api_key   = "XXX",
#'   conda_env = "/path/to/conda/env"
#' )
#' }
#'
#' @export
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python

initialize_python <- function(api_key, conda_env = NULL, python_env = NULL) {
  # --- validate inputs --------------------------------------------------------
  .check_length(api_key, 1)
  .check_input(api_key, "character")

  if (!is.null(conda_env)) {
    .check_length(conda_env, 1)
    .check_input(conda_env, "character")
  }
  if (!is.null(python_env)) {
    .check_length(python_env, 1)
    .check_input(python_env, "character")
  }

  # Require exactly one environment specification
  if (is.null(conda_env) && is.null(python_env)) {
    stop("Specify exactly one of `conda_env` or `python_env`.", call. = FALSE)
  }
  if (!is.null(conda_env) && !is.null(python_env)) {
    stop(
      "Specify only one of `conda_env` or `python_env`, not both.",
      call. = FALSE
    )
  }

  # --- configure reticulate interpreter --------------------------------------
  # Clearing RETICULATE_PYTHON avoids accidentally pinning a different interpreter.
  # Note: if the user has an explicit interpreter preference, they should set it
  # outside this function (or use reticulate configuration directly).
  Sys.unsetenv("RETICULATE_PYTHON")

  if (!is.null(conda_env)) {
    use_condaenv(conda_env, required = TRUE)
  } else {
    use_virtualenv(python_env, required = TRUE)
  }

  # --- load Python helpers and set flags -------------------------------------
  assign("api_key", api_key, envir = gt.env)

  py_file <- system.file("python/query_gtrends.py", package = "globaltrends")
  if (identical(py_file, "")) {
    stop(
      "Could not find 'python/query_gtrends.py' in the installed package.",
      call. = FALSE
    )
  }

  source_python(file = py_file, envir = gt.env)

  assign("py_setup", TRUE, envir = gt.env)
  invisible(TRUE)
}
