# initialize -------------------------------------------------------------------
test_that("initialize", {
  out <- capture_messages(initialize_db())

  expect_match(
    out,
    "Successfully created database\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'batch_keywords'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'batch_time'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'keyword_synonyms'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'data_locations'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully entered data into 'data_locations'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'batch_keywords'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'data_control'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'data_score'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully created table 'data_doi'\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully disconnected\\.",
    all = FALSE
  )
})

# start ------------------------------------------------------------------------
test_that("start1", {
  out <- capture_messages(start_db())

  expect_match(
    out,
    "Successfully connected to database\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully exported all objects to package environment gt\\.env\\.",
    all = FALSE
  )
  disconnect_db()
})

test_that("start2", {
  current_wd <- getwd()
  setwd(tempdir())
  expect_error(
    start_db(),
    "File 'db/globaltrends_db.sqlite' does not exist in working directory\\.\nSet working directory to correct path\\."
  )
  setwd(current_wd)
})

# re-create existing database --------------------------------------------------
test_that("re_create", {
  expect_error(
    initialize_db(),
    "File 'db/globaltrends_db.sqlite' already exists\\."
  )
})

# disconnect -------------------------------------------------------------------
test_that("disconnect", {
  start_db()
  expect_message(
    disconnect_db(),
    "Successfully disconnected\\."
  )
  unlink("db", recursive = TRUE)
})
