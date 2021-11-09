test_type <- function(fun, ...) {
  args <- list(...)
  expect_error(
    do.call(fun, c(args, type = "A")),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  
  expect_error(
    do.call(fun, c(args, type = list(c("obs", "sad", "trd")))),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
  
  purrr::map2(list(1, TRUE, sum), c("double", "logical", "builtin"), ~ {
    expect_error(
      do.call(fun, c(args, type = .x)),
      paste0("'type' must be object of type character.\nYou provided an object of type ", .y, ".")
    )
  }    )
}

