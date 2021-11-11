# test type --------------------------------------------------------------------
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

# test control -----------------------------------------------------------------
test_control <- function(fun, incl = 1:4, ...) {
  args <- list(...)
  
  if (1 %in% incl) {
    expect_error(
      do.call(fun, c(args, control = 1.5)),
      "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
    )
  }
  
  fun_tmp <- function(value) {
    expect_error(
      do.call(fun, c(args, control = value)),
      "Error: Batch number must be object of type integer.\nYou provided a non-integer value."
    )
  }
  
  if (2 %in% incl) fun_tmp(value = "A")
  if (3 %in% incl) fun_tmp(value = TRUE)
  if (4 %in% incl) fun_tmp(value = sum)
  
  if (5 %in% incl) {
    expect_error(
      do.call(fun, c(args, control = list(1:5))),
      "'control' must be object of length 1.\nYou provided an object of length 5."
    )
  }
  
  fun_tmp <- function(value) {
    expect_error(
      do.call(fun, c(args, control = value)),
      "no applicable method"
    )
  }
  
  if (6 %in% incl) fun_tmp(value = "A")
  if (7 %in% incl) fun_tmp(value = TRUE)
  if (8 %in% incl) fun_tmp(value = sum)
} 

# test object ------------------------------------------------------------------
test_object <- function(fun, incl = 1:4, ...) {
  args <- list(...)
  
  if (1 %in% incl) {
    expect_error(
      do.call(fun, c(args, object = 1.5)),
      "Batch number must be object of type integer.\nYou provided a non-integer numeric value."
    )
  }
  
  fun_tmp <- function(value) {
    expect_error(
      do.call(fun, c(args, object = value)),
      "Error: Batch number must be object of type integer.\nYou provided a non-integer value."
    )
  }
  
  if (2 %in% incl) fun_tmp(value = "A")
  if (3 %in% incl) fun_tmp(value = TRUE)
  if (4 %in% incl) fun_tmp(value = sum)
  
  if (5 %in% incl) {
    expect_error(
      do.call(fun, c(args, object = list(1:5))),
      "'object' must be object of length 1.\nYou provided an object of length 5."
    )
  }
  
  fun_tmp <- function(value) {
    expect_error(
      do.call(fun, c(args, object = value)),
      "no applicable method"
    )
  }
  
  if (6 %in% incl) fun_tmp(value = "A")
  if (7 %in% incl) fun_tmp(value = TRUE)
  if (8 %in% incl) fun_tmp(value = sum)
} 

# test keyword -----------------------------------------------------------------
test_keyword <- function(fun, incl = 1:3, ...) {
  args <- list(...)
  
  fun_tmp <- function(value, var_type) {
    expect_error(
      do.call(fun, c(args, keyword = value)),
      paste0("'keyword' must be object of type character.\nYou provided an object of type ", var_type, ".")
    )
  }
  
  if (1 %in% incl) fun_tmp(value = 1, var_type = "double")
  if (2 %in% incl) fun_tmp(value = TRUE, var_type = "logical")
  if (3 %in% incl) fun_tmp(value = sum, var_type = "builtin")
  
  fun_tmp <- function(value, var_type) {
    expect_error(
      do.call(fun, c(args, keyword = value)),
      "no applicable method"
    )
  }
  
  if (4 %in% incl) fun_tmp(value = 1)
  if (5 %in% incl) fun_tmp(value = TRUE)
  if (6 %in% incl) fun_tmp(value = sum)
  
} 

# test measure -----------------------------------------------------------------

# test ci ----------------------------------------------------------------------

# test locations ---------------------------------------------------------------
