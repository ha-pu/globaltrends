# Tests for initialize_python() argument validation and error paths.
#
# None of these tests require a database session or a working Python
# environment: every call errors before any Python code runs. The happy path
# (mocked reticulate) is covered in test-initialize-python.R.

# local_py_setup_state() (helper-fixtures.R) saves/restores gt.env$py_setup;
# withr::local_envvar() protects RETICULATE_PYTHON, which initialize_python()
# unsets as a side effect.

test_that("py_setup defaults to FALSE until initialize_python() succeeds", {
  local_py_setup_state()

  expect_false(gt.env$py_setup)
})

test_that("initialize_python errors on a nonexistent conda environment", {
  local_py_setup_state()
  withr::local_envvar(RETICULATE_PYTHON = NA)

  expect_error(
    initialize_python(
      api_key = "XXX",
      conda_env = "XXX"
    ),
    "Unable .* conda .*"
  )
  expect_false(gt.env$py_setup)
})

test_that("initialize_python errors on a nonexistent virtualenv", {
  local_py_setup_state()
  withr::local_envvar(RETICULATE_PYTHON = NA)

  expect_error(
    initialize_python(
      api_key = "XXX",
      python_env = "XXX"
    ),
    "Directory .*\\.virtualenvs/XXX is not a Python virtualenv"
  )
  expect_false(gt.env$py_setup)
})

test_that("initialize_python requires exactly one of conda_env or python_env", {
  local_py_setup_state()

  expect_error(
    initialize_python(api_key = "XXX"),
    "Specify exactly one of"
  )
  expect_error(
    initialize_python(api_key = "XXX", conda_env = "XXX", python_env = "XXX"),
    "Specify only one of `conda_env` or `python_env`, not both."
  )
})

test_that("initialize_python validates the api_key argument", {
  local_py_setup_state()

  expect_error(
    initialize_python(api_key = 1, conda_env = "XXX"),
    "`api_key` must be of type character"
  )
  expect_error(
    initialize_python(api_key = TRUE, conda_env = "XXX"),
    "`api_key` must be of type character"
  )
  expect_error(
    initialize_python(api_key = sum, conda_env = "XXX"),
    "`api_key` must be of type character"
  )
  expect_error(
    initialize_python(api_key = c(TRUE, TRUE), conda_env = "XXX"),
    "`api_key` must have length"
  )
})

test_that("initialize_python validates the conda_env argument", {
  local_py_setup_state()

  expect_error(
    initialize_python(conda_env = 1, api_key = "XXX"),
    "`conda_env` must be of type character"
  )
  expect_error(
    initialize_python(conda_env = TRUE, api_key = "XXX"),
    "`conda_env` must be of type character"
  )
  expect_error(
    initialize_python(conda_env = sum, api_key = "XXX"),
    "`conda_env` must be of type character"
  )
  expect_error(
    initialize_python(conda_env = c(TRUE, TRUE), api_key = "XXX"),
    "`conda_env` must have length"
  )
})

test_that("initialize_python validates the python_env argument", {
  local_py_setup_state()

  expect_error(
    initialize_python(python_env = 1, api_key = "XXX"),
    "`python_env` must be of type character"
  )
  expect_error(
    initialize_python(python_env = TRUE, api_key = "XXX"),
    "`python_env` must be of type character"
  )
  expect_error(
    initialize_python(python_env = sum, api_key = "XXX"),
    "`python_env` must be of type character"
  )
  expect_error(
    initialize_python(python_env = c(TRUE, TRUE), api_key = "XXX"),
    "`python_env` must have length"
  )
})
