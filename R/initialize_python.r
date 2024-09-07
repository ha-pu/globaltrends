#' @title Initialize gtrends Python module
#'
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate source_python

initialize_python <- function(api_code, conda_env) {
  Sys.unsetenv("RETICULATE_PYTHON")
  use_condaenv(conda_env)
  source_python(file = system.file("python/query_gtrends.py", package = "globaltrends"), envir = gt.env)
}
