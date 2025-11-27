#' @title Initialize gtrends Python module
#'
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python
#' @noRd
#' @export

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
    stop("Specify 'env'!")
  }
  assign("api_key", api_key, envir = gt.env)
  source_python(
    file = system.file("python/query_gtrends.py", package = "globaltrends"),
    envir = gt.env
  )
  assign("py_initialilzed", TRUE, envir = gt.env)
}
