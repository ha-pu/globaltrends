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

test_that("disconnect_db clears every table handle in gt.env", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  suppressMessages(start_db())

  suppressMessages(disconnect_db())

  handles <- c(
    "dt_keywords", "dt_time", "dt_control", "dt_object", "dt_score",
    "dt_doi", "dt_locations", "dt_region", "dt_related", "dt_synonyms"
  )
  for (h in handles) {
    expect_null(gt.env[[h]])
  }
})

test_that("disconnect_db errors when no session is active", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_error(
    disconnect_db(),
    "No active database session found in `gt.env`.",
    fixed = TRUE
  )
})

# internals --------------------------------------------------------------------
test_that(".save_db writes atomically and leaves no temp file behind", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())

  expect_true(file.exists(file.path("db", "globaltrends.rds")))
  expect_false(file.exists(file.path("db", "globaltrends.rds.tmp")))

  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()))
  globaltrends:::.save_db()
  expect_false(file.exists(file.path("db", "globaltrends.rds.tmp")))
})

test_that("start_db fails cleanly on a corrupt store file", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  dir.create("db")
  writeLines("this is not an RDS file", file.path("db", "globaltrends.rds"))

  expect_error(suppressMessages(start_db()))
})

test_that(".table_slot maps every table name to its gt.env handle", {
  expected <- c(
    batch_keywords   = "dt_keywords",
    batch_time       = "dt_time",
    data_control     = "dt_control",
    data_object      = "dt_object",
    data_score       = "dt_score",
    data_doi         = "dt_doi",
    data_locations   = "dt_locations",
    data_region      = "dt_region",
    data_related     = "dt_related",
    keyword_synonyms = "dt_synonyms"
  )
  for (table in names(expected)) {
    expect_identical(globaltrends:::.table_slot(table), expected[[table]])
  }
  # Every table listed in .list_files() must have a slot mapping.
  for (table in globaltrends:::.list_files()) {
    expect_type(globaltrends:::.table_slot(table), "character")
  }
})

test_that("start_db restores data.table keys after a round-trip", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  suppressMessages(start_db())
  suppressMessages(disconnect_db())
  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()))

  expect_identical(data.table::key(gt.env$dt_control), c("batch", "location"))
  expect_identical(
    data.table::key(gt.env$dt_object),
    c("batch_c", "batch_o", "location")
  )
  expect_identical(
    data.table::key(gt.env$dt_score),
    c("batch_c", "batch_o", "location")
  )
  expect_identical(
    data.table::key(gt.env$dt_doi),
    c("batch_c", "batch_o", "locations")
  )
  expect_identical(data.table::key(gt.env$dt_locations), c("type", "location"))
  expect_identical(data.table::key(gt.env$dt_region), c("batch_o", "location"))
  expect_identical(
    data.table::key(gt.env$dt_related),
    c("batch_o", "topic", "rising", "location")
  )
})

# package environment ------------------------------------------------------------
test_that("gt.env carries the bindings and defaults set by .onAttach", {
  # .onAttach() is not re-run here (it would clobber live session state);
  # instead assert the bindings it creates exist with sane types. Fixtures
  # restore any values they touch, so the config defaults still hold.
  expected_bindings <- c(
    "dt_keywords", "dt_time", "dt_control", "dt_object", "dt_score",
    "dt_doi", "dt_locations", "dt_region", "dt_related", "dt_synonyms",
    "keywords_control", "time_control", "keywords_object", "time_object",
    "keyword_synonyms", "query_wait", "py_setup", "api_calls",
    "api_calls_date", "score_calls"
  )
  expect_true(all(expected_bindings %in% names(gt.env)))

  expect_equal(gt.env$query_wait, 0.1)
  expect_type(gt.env$py_setup, "logical")
  expect_type(gt.env$api_calls, "integer")
  expect_s3_class(gt.env$api_calls_date, "Date")
  expect_type(gt.env$score_calls, "integer")
})

test_that("initialize_db seeds the default location sets", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages(initialize_db())
  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()))

  locs <- gt.env$dt_locations
  expect_setequal(unique(locs$type), c("countries", "us_states"))
  expect_setequal(locs[locs$type == "countries", ]$location, globaltrends::countries)
  expect_setequal(locs[locs$type == "us_states", ]$location, globaltrends::us_states)

  # start_db() exports each set as a named vector on gt.env.
  expect_setequal(gt.env$countries, globaltrends::countries)
  expect_setequal(gt.env$us_states, globaltrends::us_states)
})
