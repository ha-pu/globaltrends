# Snapshot tests for user-facing message streams. These lock in the exact
# wording and sequence of the multi-line progress/cascade messages that
# string-level expect_match() assertions elsewhere only spot-check.
# Review changes with testthat::snapshot_review() after intentional rewording.

test_that("add_control_keyword reports each auto-split batch", {
  local_db()

  expect_snapshot({
    add_control_keyword(
      keyword = c("gmail", "maps", "news", "translate", "weather", "wikipedia", "youtube"),
      start_date = "2016-01",
      end_date   = "2019-12"
    )
  })
})

test_that("add_object_keyword reports each auto-split batch", {
  local_db()

  expect_snapshot({
    add_object_keyword(
      keyword = c("amazon", "apple", "facebook", "google", "microsoft", "netflix", "twitter"),
      start_date = "2016-01",
      end_date   = "2019-12"
    )
  })
})

test_that("database lifecycle messages are stable", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_snapshot({
    initialize_db()
    initialize_db()
    start_db()
    disconnect_db()
  })
})

test_that("download_control reports per-location progress [mocked]", {
  local_db()
  setup_keywords()
  local_mocked_bindings(.get_trend = make_trend_data, .package = "globaltrends")

  expect_snapshot({
    download_control(control = 1, locations = c("US", "CN", "JP"))
    download_control(control = 1, locations = "US")
  })
})

test_that("remove_data(batch_keywords) reports the full deletion cascade", {
  local_cascade_db()

  expect_snapshot({
    remove_data(table = "batch_keywords", control = 1)
  })
})

test_that("add_locations reports additions, duplicates, and the Namibia drop", {
  local_db()

  expect_snapshot({
    add_locations(locations = c("AT", "CH", "DE"), type = "DACH")
    add_locations(locations = c("AT", "BE"), type = "DACH")
    add_locations(locations = c("AT", "NA"), type = "DACH")
  })
})
