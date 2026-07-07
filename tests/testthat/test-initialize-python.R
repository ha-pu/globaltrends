# Happy-path tests for initialize_python() with the reticulate seams mocked
# (use_condaenv/use_virtualenv/source_python are imported bindings, so
# local_mocked_bindings() can replace them inside the globaltrends namespace).
# No real Python environment is required. Error paths live in test-python.R.
# local_python_init_state() lives in helper-fixtures.R.

test_that("initialize_python configures a conda environment and activates the backend", {
  local_python_init_state()
  withr::local_envvar(RETICULATE_PYTHON = "C:/some/pinned/python.exe")

  conda_calls <- list()
  sourced_files <- character()
  local_mocked_bindings(
    use_condaenv = function(condaenv, required = FALSE, ...) {
      conda_calls[[length(conda_calls) + 1]] <<- list(
        condaenv = condaenv, required = required
      )
      invisible(NULL)
    },
    source_python = function(file, envir = NULL, ...) {
      sourced_files <<- c(sourced_files, file)
      invisible(NULL)
    },
    .package = "globaltrends"
  )

  out <- initialize_python(api_key = "MY_KEY", conda_env = "my-conda-env")

  expect_true(out)
  expect_true(gt.env$py_setup)
  expect_equal(gt.env$api_key, "MY_KEY")

  # The pinned interpreter env var is cleared before configuring the env.
  expect_equal(Sys.getenv("RETICULATE_PYTHON"), "")

  expect_length(conda_calls, 1)
  expect_equal(conda_calls[[1]]$condaenv, "my-conda-env")
  expect_true(conda_calls[[1]]$required)

  # The package's Python helper is sourced into gt.env.
  expect_length(sourced_files, 1)
  expect_match(sourced_files, "query_gtrends\\.py$")
})

test_that("initialize_python configures a virtualenv when python_env is given", {
  local_python_init_state()

  venv_calls <- character()
  local_mocked_bindings(
    use_virtualenv = function(virtualenv, required = FALSE, ...) {
      venv_calls <<- c(venv_calls, virtualenv)
      invisible(NULL)
    },
    source_python = function(file, envir = NULL, ...) invisible(NULL),
    .package = "globaltrends"
  )

  out <- initialize_python(api_key = "MY_KEY", python_env = "/path/to/venv")

  expect_true(out)
  expect_true(gt.env$py_setup)
  expect_equal(venv_calls, "/path/to/venv")
})

test_that("initialize_python leaves py_setup FALSE when the environment setup fails", {
  local_python_init_state()
  gt.env$py_setup <- FALSE

  local_mocked_bindings(
    use_condaenv = function(...) stop("Unable to locate conda environment"),
    .package = "globaltrends"
  )

  expect_error(
    initialize_python(api_key = "MY_KEY", conda_env = "broken"),
    "Unable to locate conda environment"
  )
  expect_false(gt.env$py_setup)
})
