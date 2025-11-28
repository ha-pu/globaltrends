# setup ------------------------------------------------------------------------
source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# test parameter ---------------------------------------------------------------
test_that("initialize_py1", {
  expect_false(
    gt.env$py_initialized
  )
})

test_that("initialize_py2", {
  expect_error(
    initialize_python(
      api_key = "XXX",
      conda_env = "XXX"
    ),
    "Unable .* conda .*"
  )
})

test_that("initialize_py3", {
  expect_error(
    initialize_python(
      api_key = "XXX",
      python_env = "XXX"
    ),
    "Directory ~/.virtualenvs/XXX is not a Python virtualenv"
  )
})

test_that("initialize_py4", {
  expect_error(
    initialize_python(
      api_key = "XXX"
    ),
    "Specify 'env'!"
  )
})

test_that("signals1", {
  expect_error(
    initialize_python(api_key = 1, conda_env = "XXX"),
    "'api_key' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    initialize_python(api_key = TRUE, conda_env = "XXX"),
    "'api_key' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    initialize_python(api_key = sum, conda_env = "XXX"),
    "'api_key' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    initialize_python(api_key = c(TRUE, TRUE), conda_env = "XXX"),
    "'api_key' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("signals2", {
  expect_error(
    initialize_python(conda_env = 1, api_key = "XXX"),
    "'conda_env' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    initialize_python(conda_env = TRUE, api_key = "XXX"),
    "'conda_env' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    initialize_python(conda_env = sum, api_key = "XXX"),
    "'conda_env' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    initialize_python(conda_env = c(TRUE, TRUE), api_key = "XXX"),
    "'conda_env' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("signals3", {
  expect_error(
    initialize_python(python_env = 1, api_key = "XXX"),
    "'python_env' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    initialize_python(python_env = TRUE, api_key = "XXX"),
    "'python_env' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    initialize_python(python_env = sum, api_key = "XXX"),
    "'python_env' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    initialize_python(python_env = c(TRUE, TRUE), api_key = "XXX"),
    "'python_env' must be object of length 1.\nYou provided an object of length 2."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
