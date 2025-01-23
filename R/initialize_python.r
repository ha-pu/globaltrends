#' @title Initialize gtrends Python module
#'
#' @importFrom reticulate use_condaenv
#' @importFrom reticulate source_python
#' @noRd
#' @export

initialize_python <- function(api_key, conda_env) {
  Sys.unsetenv("RETICULATE_PYTHON")
  use_condaenv(conda_env)
  assign("api_key", api_key, envir = gt.env)
  source_python(file = system.file("python/query_gtrends.py", package = "globaltrends"), envir = gt.env)
}
