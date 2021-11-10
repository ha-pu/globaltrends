test_type <- function(fun, incl = 1:5, ...) {
  args <- list(...)

  if (1 %in% incl) {
    expect_error(
      do.call(fun, c(args, type = list(c("obs", "sad", "trd")))),
      "'type' must be object of length 1.\nYou provided an object of length 3."
    )
  }
  
  if (2 %in% incl) {
    expect_error(
      do.call(fun, c(args, type = "A")),
      "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
    )
  }

  fun_tmp <- function(value, var_type) {
    expect_error(
      do.call(fun, c(args, type = value)),
      paste0("'type' must be object of type character.\nYou provided an object of type ", var_type, ".")
    )
  }

  if (3 %in% incl) fun_tmp(value = 1, var_type = "double")

  if (4 %in% incl) fun_tmp(value = TRUE, var_type = "logical")

  if (5 %in% incl) fun_tmp(value = sum, var_type = "builtin")
}
