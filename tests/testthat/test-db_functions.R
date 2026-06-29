# initialize -------------------------------------------------------------------
test_that("initialize", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  out <- capture_messages(initialize_db())

  expect_match(
    out,
    "Database files created successfully under 'db/'\\.",
    all = FALSE
  )
})

# start ------------------------------------------------------------------------
test_that("start1", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  withr::defer(suppressMessages(disconnect_db()))

  out <- capture_messages(start_db())

  expect_match(
    out,
    "Successfully loaded database and exported table handles to gt\\.env\\.",
    all = FALSE
  )
  expect_false(is.null(gt.env$dt_control))
  expect_false(is.null(gt.env$dt_keywords))
  expect_false(is.null(gt.env$keywords_control))
})

test_that("start2", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_error(
    start_db(),
    "Database files do not exist under 'db/'\\."
  )
})

# re-create existing database --------------------------------------------------
test_that("re_create", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())

  expect_message(
    initialize_db(),
    "Database files already exist under 'db/'\\."
  )
})

# rds round-trip ---------------------------------------------------------------
test_that("rds_roundtrip", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  suppressMessages(start_db())

  new_row <- data.table::data.table(
    type = "control", batch = 1L, keyword = "roundtrip_keyword"
  )
  gt.env$dt_keywords <- data.table::rbindlist(
    list(gt.env$dt_keywords, new_row), use.names = TRUE
  )

  suppressMessages(disconnect_db())
  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()))

  result <- gt.env$dt_keywords[gt.env$dt_keywords$keyword == "roundtrip_keyword", ]

  expect_equal(nrow(result), 1L)
  expect_equal(result$keyword, "roundtrip_keyword")
  expect_equal(result$type, "control")
  expect_equal(result$batch, 1L)
})

# disconnect -------------------------------------------------------------------
test_that("disconnect", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  suppressMessages(start_db())

  expect_message(
    disconnect_db(),
    "Successfully disconnected and persisted database to 'db/'\\."
  )
})
