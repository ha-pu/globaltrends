# Tests for add_locations()
# local_db() is provided by helper-db.R and auto-loaded by testthat.

# creating sets and respecting export flag -------------------------------------
test_that("add_loc1", {
  local_db()

  expect_message(
    add_locations(
      locations = c("AT", "DE", "CH"),
      type = "dach",
      export = FALSE
    ),
    "Successfully created/extended location set 'dach' with 3 location(s) (AT, DE, CH).",
    fixed = TRUE
  )
  # export = FALSE: gt.env$dach must not be assigned yet
  expect_null(gt.env$dach)

  expect_message(
    add_locations(
      locations = c("CN", "JP"),
      type = "asia",
      export = TRUE
    ),
    "Successfully created/extended location set 'asia' with 2 location(s) (CN, JP).",
    fixed = TRUE
  )
  # export = TRUE: gt.env$asia is available immediately
  expect_identical(gt.env$asia, c("CN", "JP"))
})

# persistence across disconnect / reconnect ------------------------------------
test_that("add_loc2", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)
  suppressMessages({
    initialize_db()
    start_db()
    add_locations(locations = c("AT", "DE", "CH"), type = "dach", export = FALSE)
    add_locations(locations = c("CN", "JP"), type = "asia", export = FALSE)
    disconnect_db()
    start_db() # start_db() calls .export_locations() on reconnect
  })
  withr::defer(suppressMessages(disconnect_db()))

  expect_setequal(gt.env$dach, c("AT", "DE", "CH"))
  expect_setequal(gt.env$asia, c("CN", "JP"))
})

# duplicate prevention ---------------------------------------------------------
test_that("duplicate prevention: full overlap returns empty tibble", {
  local_db()
  suppressMessages(
    add_locations(locations = c("AT", "DE", "CH"), type = "dach", export = FALSE)
  )

  expect_message(
    result <- add_locations(
      locations = c("AT", "DE", "CH"),
      type = "dach",
      export = FALSE
    ),
    "No new locations added for set 'dach'. All provided locations already exist (AT, DE, CH).",
    fixed = TRUE
  )
  expect_identical(nrow(result), 0L)
})

test_that("duplicate prevention: partial overlap adds only new codes", {
  local_db()
  suppressMessages(
    add_locations(locations = c("AT", "DE"), type = "dach", export = FALSE)
  )

  expect_message(
    add_locations(
      locations = c("AT", "DE", "CH"),
      type = "dach",
      export = FALSE
    ),
    "Location set 'dach': added 1 location(s) (CH); skipped 2 existing (AT, DE).",
    fixed = TRUE
  )
})

# invalid location code --------------------------------------------------------
test_that("invalid1", {
  local_db()
  expect_error(
    add_locations("test", "test"),
    "Invalid location code(s): test. Valid codes must appear in `gtrendsR::countries$country_code` or `gtrendsR::countries$sub_code`.",
    fixed = TRUE
  )
})

# Namibia API limitation -------------------------------------------------------
test_that("namibia1", {
  local_db()
  expect_warning(
    add_locations(c("NA", "AT"), "test"),
    "The Google Trends API cannot handle the location code 'NA' (Namibia).",
    fixed = TRUE
  )
})

test_that("namibia2", {
  local_db()
  expect_error(
    add_locations("NA", "test"),
    "The Google Trends API cannot handle the location code 'NA' (Namibia). It was dropped, leaving `locations` empty.",
    fixed = TRUE
  )
})

# input validation: locations argument type ------------------------------------
test_that("signals1", {
  local_db()
  expect_error(
    add_locations(locations = 1, type = "A"),
    "Error: `locations` must be of type character.",
    fixed = TRUE
  )
  expect_error(
    add_locations(locations = TRUE, type = "A"),
    "Error: `locations` must be of type character.",
    fixed = TRUE
  )
  expect_error(
    add_locations(locations = sum, type = "A"),
    "Error: `locations` must be of type character.",
    fixed = TRUE
  )
})

# input validation: export argument type and length ----------------------------
test_that("signals3", {
  local_db()
  expect_error(
    add_locations(locations = "A", type = "A", export = 1),
    "Error: `export` must be of type logical.",
    fixed = TRUE
  )
  expect_error(
    add_locations(locations = "A", type = "A", export = "A"),
    "Error: `export` must be of type logical.",
    fixed = TRUE
  )
  expect_error(
    add_locations(locations = "A", type = "A", export = sum),
    "Error: `export` must be of type logical.",
    fixed = TRUE
  )
  expect_error(
    add_locations(locations = "A", type = "A", export = c(TRUE, TRUE)),
    "Error: `export` must have length <= 1.",
    fixed = TRUE
  )
})
