# Regression tests for remove_data() selectivity.
#
# These specifically guard against a class of bug where a `remove_data()`
# internal helper's argument shares a name with the column it filters on
# (e.g. `batch_o` argument vs. `batch_o` column). Under data.table's NSE
# (active via `.datatable.aware` in zzz.r), a bare symbol that matches a
# column name resolves to the COLUMN rather than the argument, turning the
# filter into an always-true/always-false tautology and deleting every row
# instead of just the targeted batch. Asserting only that the *targeted*
# batch is gone does not catch this (an always-true filter also leaves zero
# targeted rows); these tests additionally assert that *other* batches'
# data survives untouched.

test_that("remove_data(batch_keywords) only deletes the targeted control batch", {
  local_db()
  suppressMessages({
    add_control_keyword(keyword = "k1", start_date = "2010-01", end_date = "2010-02")
    add_control_keyword(keyword = "k2", start_date = "2010-01", end_date = "2010-02")
  })

  suppressMessages(remove_data(table = "batch_keywords", control = 2))

  dt_kw <- gt.env$dt_keywords
  dt_tm <- gt.env$dt_time
  expect_equal(nrow(dt_kw[dt_kw$type == "control" & dt_kw$batch == 1L, ]), 1L)
  expect_equal(nrow(dt_kw[dt_kw$type == "control" & dt_kw$batch == 2L, ]), 0L)
  expect_equal(nrow(dt_tm[dt_tm$type == "control" & dt_tm$batch == 1L, ]), 1L)
  expect_equal(nrow(dt_tm[dt_tm$type == "control" & dt_tm$batch == 2L, ]), 0L)
})

test_that("remove_data(data_object) with control+object only deletes the targeted batch pair", {
  local_db()
  suppressMessages({
    add_control_keyword(keyword = "k1", start_date = "2010-01", end_date = "2010-02")
    add_object_keyword(keyword = "o1", start_date = "2010-01", end_date = "2010-02")
  })

  gt.env$dt_object <- data.table::rbindlist(list(
    gt.env$dt_object,
    data.table::data.table(
      location = "US", keyword = "o1", date = 1, hits = 1,
      batch_c = 1L, batch_o = 1L
    ),
    data.table::data.table(
      location = "US", keyword = "o1", date = 1, hits = 1,
      batch_c = 1L, batch_o = 2L
    )
  ), use.names = TRUE)

  suppressMessages(remove_data(table = "data_object", control = 1, object = 2))

  dt <- gt.env$dt_object
  expect_equal(nrow(dt[dt$batch_c == 1L & dt$batch_o == 1L, ]), 1L)
  expect_equal(nrow(dt[dt$batch_c == 1L & dt$batch_o == 2L, ]), 0L)
})

test_that("remove_data(data_region/data_related) only deletes the targeted object batch", {
  local_db()
  suppressMessages(add_object_keyword(
    keyword = "o1",
    start_date = "2010-01",
    end_date = "2010-02"
  ))

  gt.env$dt_region <- data.table::rbindlist(list(
    gt.env$dt_region,
    data.table::data.table(
      term = "o1", location = "world",
      start_date = 1, end_date = 2,
      region_code = "X", region_name = "X", hits = 1, batch_o = 1L
    ),
    data.table::data.table(
      term = "o1", location = "world",
      start_date = 1, end_date = 2,
      region_code = "X", region_name = "X", hits = 1, batch_o = 2L
    )
  ), use.names = TRUE)

  suppressMessages(remove_data(table = "data_region", object = 2))

  dt <- gt.env$dt_region
  expect_equal(nrow(dt[dt$batch_o == 1L, ]), 1L)
  expect_equal(nrow(dt[dt$batch_o == 2L, ]), 0L)
})
