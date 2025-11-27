#' @title Initialize gtrends Python module
#'
#' @description
#' The function initializes the Python session required to download data from
#' Google Trends using the Google Trends research API. The user can define an
#' API key and either a Conda or Python environment.
#'
#' @details
#' To use this API acces, you must:
#' 1. Apply for API access [online](https://support.google.com/trends/contact/trends_api).
#'   and generate an API key in your Google developer console.
#' 2. Create a Python or Conda environment where you install the
#'  `google-api-python-client` package with pip.
#'
#' @param api_key API key obtained from Google. Object of type `character`.
#'
#' @param conda_env Storage location of a Conda environment. Object of type
#' `character`.
#'
#' @param python_env Storage location of a Python environment. Object of type
#' `character`.
#'
#' @return
#' The functions sets the environment variable `gt.env` to `TRUE`. This channels
#' downloads to the API. Otherwise, `globaltrends` uses `gtrendsR::gtrends` for
#' downloads.
#'
#' @examples
#' \dontrun{
#' initialize_python(
#'   api_key = "XXX",
#'   conda_env = "/xxx/venv"
#' )
#' }
#'
#' @rdname initialize_python
#' @export
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python

initialize_python <- function(api_key, conda_env = NULL, python_env = NULL) {
  .check_length(api_key, 1)
  .check_input(api_key, "character")
  Sys.unsetenv("RETICULATE_PYTHON")
  if (!is.null(conda_env)) {
    .check_length(conda_env, 1)
    .check_input(conda_env, "character")
    use_condaenv(conda_env)
  } else if (!is.null(python_env)) {
    .check_length(python_env, 1)
    .check_input(python_env, "character")
    use_virtualenv(python_env)
  } else {
    stop("Error: Specify 'env'!")
  }
  assign("api_key", api_key, envir = gt.env)
  source_python(
    file = system.file("python/query_gtrends.py", package = "globaltrends"),
    envir = gt.env
  )
  assign("py_initialilzed", TRUE, envir = gt.env)
}
