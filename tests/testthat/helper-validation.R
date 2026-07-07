# Reusable argument-validation batteries. Each helper calls `fun` with a set
# of invalid values for one argument and asserts the exact validation error.
# `incl` selects which cases run (see the numbered branches below); `...` is
# passed through to `fun` as fixed arguments.

# Short aliases for the internal validators under test in
# test-check-functions.R.
check_input <- globaltrends:::.check_input
check_length <- globaltrends:::.check_length
check_locations <- globaltrends:::.check_locations
check_batch <- globaltrends:::.check_batch

# test control -----------------------------------------------------------------
test_control <- function(fun, incl = 1:4, ...) {
  args <- list(...)

  if (1 %in% incl) {
    expect_error(
      do.call(fun, c(args, control = 1.5)),
      "Batch id must be an integer value.\nYou provided a non-integer numeric value."
    )
  }

  fun_tmp <- function(value) {
    expect_error(
      do.call(fun, c(args, control = value)),
      "Batch id must be an integer value.\nYou provided an object of type "
    )
  }

  if (2 %in% incl) fun_tmp(value = "A")
  if (3 %in% incl) fun_tmp(value = TRUE)
  if (4 %in% incl) fun_tmp(value = sum)

  if (5 %in% incl) {
    expect_error(
      do.call(fun, c(args, control = list(1:5))),
      "must have length <= 1.\nYou provided an object of length 5."
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
      "Batch id must be an integer value.\nYou provided a non-integer numeric value."
    )
  }

  fun_tmp <- function(value) {
    expect_error(
      do.call(fun, c(args, object = value)),
      "Batch id must be an integer value.\nYou provided an object of type "
    )
  }

  if (2 %in% incl) fun_tmp(value = "A")
  if (3 %in% incl) fun_tmp(value = TRUE)
  if (4 %in% incl) fun_tmp(value = sum)

  if (5 %in% incl) {
    expect_error(
      do.call(fun, c(args, object = list(1:5))),
      "must have length <= 1.\nYou provided an object of length 5."
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
      paste0("`keyword` must be of type character.\nYou provided an object of type ", var_type, ".")
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

# test locations ---------------------------------------------------------------
test_locations <- function(fun, incl = FALSE, ...) {
  args <- list(...)

  fun_tmp <- function(value, var_type) {
    expect_error(
      do.call(fun, c(args, locations = value)),
      paste0("must be of type character.\nYou provided an object of type ", var_type, ".")
    )
  }

  Map(fun_tmp, list(1, TRUE, sum), c("double", "logical", "builtin"))

  if (incl) {
    expect_error(
      do.call(fun, c(args, locations = list(letters[1:5]))),
      "must have length <= 1.\nYou provided an object of length 5."
    )
  }
}
