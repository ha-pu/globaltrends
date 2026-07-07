# Tests for add_synonym() and aggregate_synonyms().
# Helpers local_db() and local_synonyms_db() are provided by helper-db.R
# and auto-loaded by testthat before this file runs.

# add_synonym() ----------------------------------------------------------------
test_that("add_synonyms1", {
  local_db()

  out <- capture_messages(
    add_synonym(
      keyword = "fc bayern",
      synonym = c("bayern munich", "bayern munchen")
    )
  )

  expect_match(
    out,
    "Successfully added synonym | keyword: fc bayern | synonym: bayern munich\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully added synonym | keyword: fc bayern | synonym: bayern munchen\\.",
    all = FALSE
  )
  expect_equal(nrow(gt.env$keyword_synonyms), 2)
})

# aggregate_synonyms() ---------------------------------------------------------
test_that("aggregate_synonyms1", {
  local_synonyms_db()
  suppressMessages(
    add_synonym(keyword = "fc bayern", synonym = c("bayern munich", "bayern munchen"))
  )

  out <- capture_messages(aggregate_synonyms(control = 1))
  expect_match(out, "Successfully aggregated synonyms", all = FALSE)
})

test_that("aggregate_synonyms2", {
  local_synonyms_db()
  suppressMessages(
    add_synonym(keyword = "fc bayern", synonym = c("bayern munich", "bayern munchen"))
  )

  out <- capture_messages(aggregate_synonyms(control = 1))
  expect_match(out, "Successfully aggregated synonyms", all = FALSE)
})

test_that("aggregate_synonyms_no_data", {
  local_db()
  suppressMessages({
    add_object_keyword(keyword = "kw_a", start_date = "2020-01", end_date = "2020-01")
    add_object_keyword(keyword = "kw_b", start_date = "2020-01", end_date = "2020-01")
    add_synonym(keyword = "kw_a", synonym = "kw_b")
  })

  # synonym mapping exists but data_score is empty → "No score data found" early exit
  out <- capture_messages(aggregate_synonyms(control = 1))

  expect_match(out, "No score data found", all = FALSE)
  expect_equal(nrow(gt.env$dt_score), 0L)
})

# score comparison -------------------------------------------------------------
test_that("keyword_score", {
  local_synonyms_db()
  suppressMessages(
    add_synonym(keyword = "fc bayern", synonym = c("bayern munich", "bayern munchen"))
  )

  score_before <- export_score(keyword = "fc bayern")
  suppressMessages(aggregate_synonyms(control = 1))
  score_after <- export_score(keyword = "fc bayern")

  # CN appears in both batch 1 (canonical) and batch 2 (synonyms), so the
  # mean score must increase after aggregation.
  before_cn <- score_before[score_before$location == "CN", ]
  after_cn <- score_after[score_after$location == "CN", ]
  expect_gt(mean(after_cn$score, na.rm = TRUE), mean(before_cn$score, na.rm = TRUE))

  # JP had no canonical (batch 1) data before aggregation; synonym batch 2
  # covers JP, so aggregation must introduce rows there.
  expect_equal(nrow(score_before[score_before$location == "JP", ]), 0L)
  expect_gt(nrow(score_after[score_after$location == "JP", ]), 0L)
})

test_that("aggregate_synonyms_exact_score", {
  local_db()
  suppressMessages({
    add_object_keyword(keyword = "kw_a", start_date = "2020-01", end_date = "2020-01")
    add_object_keyword(keyword = "kw_b", start_date = "2020-01", end_date = "2020-01")
  })

  # Insert synthetic scores with known values: canonical = 10, synonym = 5.
  # After aggregation the merged canonical row must equal 10 + 5 = 15.
  gt.env$dt_score <- data.table::rbindlist(list(
    gt.env$dt_score,
    data.table::data.table(
      location = "US", keyword = "kw_a",
      date = as.Date("2020-01-01"), score = 10,
      batch_c = 1L, batch_o = 1L
    )
  ), use.names = TRUE)
  gt.env$dt_score <- data.table::rbindlist(list(
    gt.env$dt_score,
    data.table::data.table(
      location = "US", keyword = "kw_b",
      date = as.Date("2020-01-01"), score = 5,
      batch_c = 1L, batch_o = 2L
    )
  ), use.names = TRUE)

  suppressMessages({
    add_synonym(keyword = "kw_a", synonym = "kw_b")
    aggregate_synonyms(control = 1)
  })

  tmp <- export_score(keyword = "kw_a")
  result <- tmp[tmp$location == "US", ]
  expect_equal(nrow(result), 1L)
  expect_equal(result$score, 15)
})

test_that("aggregate_synonyms is a no-op when no synonyms are registered", {
  local_db()

  expect_message(
    out <- aggregate_synonyms(control = 1),
    "No synonym mappings found in the database. Nothing to aggregate.",
    fixed = TRUE
  )
  expect_equal(out, data.frame())
})

test_that("aggregate_synonyms is a no-op when the synonym is not an object keyword", {
  local_db()
  suppressMessages({
    add_object_keyword(keyword = "kw_a", start_date = "2020-01", end_date = "2020-01")
    # "kw_x" is registered as a synonym but never added as an object keyword,
    # so it cannot be mapped to an object batch.
    add_synonym(keyword = "kw_a", synonym = "kw_x")
  })

  expect_message(
    out <- aggregate_synonyms(control = 1),
    "No synonym mappings found in the database. Nothing to aggregate.",
    fixed = TRUE
  )
  expect_equal(out, data.frame())
})

# add_synonym() input validation -----------------------------------------------
test_that("add_synonyms2", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(
    add_synonym(keyword = letters[1:2], synonym = LETTERS[1:2]),
    "must have length <= 1"
  )
})

test_that("add_synonyms4", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(add_synonym(keyword = 1, synonym = "A"), "must be of type character")
  expect_error(add_synonym(keyword = TRUE, synonym = "A"), "must be of type character")
  expect_error(add_synonym(keyword = sum, synonym = "A"), "must be of type character")
})

test_that("add_synonyms5", {
  withr::local_envvar(LANGUAGE = "EN")
  # unlist() passes non-list input through unchanged, so .check_input() on the
  # internal `synonyms` variable is always the validation path that fires.
  expect_error(
    add_synonym(keyword = "A", synonym = 1),
    "Error: `synonyms` must be of type character.\nYou provided an object of type double.",
    fixed = TRUE
  )
  expect_error(
    add_synonym(keyword = "A", synonym = TRUE),
    "Error: `synonyms` must be of type character.\nYou provided an object of type logical.",
    fixed = TRUE
  )
  expect_error(
    add_synonym(keyword = "A", synonym = sum),
    "Error: `synonyms` must be of type character.\nYou provided an object of type builtin.",
    fixed = TRUE
  )
})

# aggregate_synonyms() input validation ----------------------------------------
test_that("aggregate_synonyms3", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(aggregate_synonyms(control = 1.5), "non-integer")
  expect_error(aggregate_synonyms(control = "A"), "Batch id must be an integer")
  expect_error(aggregate_synonyms(control = TRUE), "Batch id must be an integer")
  expect_error(aggregate_synonyms(control = sum), "Batch id must be an integer")
})

