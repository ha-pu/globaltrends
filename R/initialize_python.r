#' @title Initialize gtrends Python module
#'
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python
#' @noRd
#' @export

initialize_python <- function(api_key, conda_env = NULL, python_env = NULL) {
  Sys.unsetenv("RETICULATE_PYTHON")
  if (!is.null(conda_env)) {
    use_condaenv(conda_env)
  } else if (!is.null(python_env)) {
    use_virtualenv(python_env)
  } else {
    error("Specify 'env'!")
  }
  assign("api_key", api_key, envir = gt.env)
  source_python(
    file = system.file("python/query_gtrends.py", package = "globaltrends"),
    envir = gt.env
  )
  assign("py_initialilzed", TRUE, envir = gt.env)
}
