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
  expect_false(is.null(gt.env$globaltrends_db))
  expect_false(is.null(gt.env$tbl_control))
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

# parquet round-trip -----------------------------------------------------------
test_that("parquet_roundtrip", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  suppressMessages(start_db())

  DBI::dbExecute(
    gt.env$globaltrends_db,
    "INSERT INTO batch_keywords VALUES ('control', 1, 'roundtrip_keyword')"
  )

  suppressMessages(disconnect_db())
  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()))

  result <- DBI::dbGetQuery(
    gt.env$globaltrends_db,
    "SELECT * FROM batch_keywords WHERE keyword = 'roundtrip_keyword'"
  )

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
