# Tests for the seven export_* functions.
#
# Every test builds its own isolated database via local_export_db() (see
# helper-db.R), which seeds dt_control/dt_object/dt_score/dt_doi with the
# example_* datasets: control batch 1 and object batches 1:3 for locations
# US, CN, and world (DOI for the "countries" set). Expected row counts follow
# from 120 months (2010-01 to 2019-12) per (keyword, location):
#   data_control: 5 keywords x 120 months x 2 non-world locations = 1200
#   data_object:  13 keywords x 120 months x 2 non-world locations = 3120
#   data_score:    8 keywords x 120 months x 2 non-world locations = 1920
#   data_doi:     10 keywords x 120 months (countries set)          = 1200
# The corresponding *_global counts use the single world location instead.

# export_control ---------------------------------------------------------------
test_that("export_control returns all non-world control rows with Date dates", {
  local_export_db()
  out <- export_control()
  expect_equal(nrow(out), 1200)
  expect_s3_class(out$date, "Date")
  expect_false(any(out$location == "world"))
  expect_named(out, c("location", "keyword", "date", "hits", "control"))
})

test_that("export_control filters by control batch", {
  local_export_db()
  expect_equal(nrow(export_control(control = 1)), 1200)
})

test_that("export_control treats control = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_control(control = NULL)), 1200)
})

test_that("export_control accepts a list control filter", {
  local_export_db()
  expect_equal(nrow(export_control(control = list(1))), 1200)
})

test_that("export_control validates the control argument", {
  local_export_db()
  test_control(fun = export_control)
})

# export_control_global --------------------------------------------------------
test_that("export_control_global returns only world rows", {
  local_export_db()
  out <- export_control_global()
  expect_equal(nrow(out), 600)
  expect_s3_class(out$date, "Date")
  expect_true(all(out$location == "world"))
})

test_that("export_control_global filters by control batch", {
  local_export_db()
  expect_equal(nrow(export_control_global(control = 1)), 600)
})

test_that("export_control_global treats control = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_control_global(control = NULL)), 600)
})

test_that("export_control_global accepts a list control filter", {
  local_export_db()
  expect_equal(nrow(export_control_global(control = list(1))), 600)
})

test_that("export_control_global validates the control argument", {
  local_export_db()
  test_control(fun = export_control_global)
})

# export_object ----------------------------------------------------------------
test_that("export_object returns all non-world object rows with renamed batch columns", {
  local_export_db()
  out <- export_object()
  expect_equal(nrow(out), 3120)
  expect_s3_class(out$date, "Date")
  expect_true(all(c("control", "object") %in% names(out)))
  expect_false(any(c("batch_c", "batch_o") %in% names(out)))
})

test_that("export_object filters by keyword", {
  local_export_db()
  expect_equal(nrow(export_object(keyword = "manchester united")), 240)
})

test_that("export_object treats keyword = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_object(keyword = NULL)), 3120)
})

test_that("export_object accepts a list of keywords", {
  local_export_db()
  out <- export_object(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 480)
})

test_that("export_object validates the keyword argument", {
  local_export_db()
  test_keyword(fun = export_object)
})

test_that("export_object validates object and accepts a vector filter", {
  local_export_db()
  test_object(fun = export_object)
  expect_equal(nrow(export_object(object = 1:5)), 3120)
})

test_that("export_object validates control and accepts a vector filter", {
  local_export_db()
  test_control(fun = export_object)
  expect_equal(nrow(export_object(control = 1:5)), 3120)
})

# export_object_global ---------------------------------------------------------
test_that("export_object_global returns only world rows", {
  local_export_db()
  out <- export_object_global()
  expect_equal(nrow(out), 1560)
  expect_s3_class(out$date, "Date")
  expect_true(all(out$location == "world"))
})

test_that("export_object_global filters by keyword", {
  local_export_db()
  expect_equal(nrow(export_object_global(keyword = "manchester united")), 120)
})

test_that("export_object_global treats keyword = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_object_global(keyword = NULL)), 1560)
})

test_that("export_object_global accepts a list of keywords", {
  local_export_db()
  out <- export_object_global(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 240)
})

test_that("export_object_global validates the keyword argument", {
  local_export_db()
  test_keyword(fun = export_object_global)
})

test_that("export_object_global validates object and accepts a vector filter", {
  local_export_db()
  test_object(fun = export_object_global)
  expect_equal(nrow(export_object_global(object = 1:5)), 1560)
})

test_that("export_object_global validates control and accepts a vector filter", {
  local_export_db()
  test_control(fun = export_object_global)
  expect_equal(nrow(export_object_global(control = 1:5)), 1560)
})

# export_score -----------------------------------------------------------------
test_that("export_score returns all non-world score rows with renamed batch columns", {
  local_export_db()
  out <- export_score()
  expect_equal(nrow(out), 1920)
  expect_s3_class(out$date, "Date")
  expect_true(all(c("control", "object") %in% names(out)))
  expect_false(any(c("batch_c", "batch_o") %in% names(out)))
})

test_that("export_score filters by keyword", {
  local_export_db()
  expect_equal(nrow(export_score(keyword = "manchester united")), 240)
})

test_that("export_score treats keyword = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_score(keyword = NULL)), 1920)
})

test_that("export_score accepts a list of keywords", {
  local_export_db()
  out <- export_score(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 480)
})

test_that("export_score validates the keyword argument", {
  local_export_db()
  test_keyword(fun = export_score)
})

test_that("export_score validates object and accepts a vector filter", {
  local_export_db()
  test_object(fun = export_score)
  expect_equal(nrow(export_score(object = 1:5)), 1920)
})

test_that("export_score validates control and accepts a vector filter", {
  local_export_db()
  test_control(fun = export_score)
  expect_equal(nrow(export_score(control = 1:5)), 1920)
})

# export_voi -------------------------------------------------------------------
test_that("export_voi returns only world score rows", {
  local_export_db()
  out <- export_voi()
  expect_equal(nrow(out), 1200)
  expect_s3_class(out$date, "Date")
  expect_true(all(out$location == "world"))
})

test_that("export_voi filters by keyword", {
  local_export_db()
  expect_equal(nrow(export_voi(keyword = "manchester united")), 120)
})

test_that("export_voi treats keyword = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_voi(keyword = NULL)), 1200)
})

test_that("export_voi accepts a list of keywords", {
  local_export_db()
  out <- export_voi(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 240)
})

test_that("export_voi validates the keyword argument", {
  local_export_db()
  test_keyword(fun = export_voi)
})

test_that("export_voi validates object and accepts a vector filter", {
  local_export_db()
  test_object(fun = export_voi)
  expect_equal(nrow(export_voi(object = 1:5)), 1200)
})

test_that("export_voi validates control and accepts a vector filter", {
  local_export_db()
  test_control(fun = export_voi)
  expect_equal(nrow(export_voi(control = 1:5)), 1200)
})

# export_doi -------------------------------------------------------------------
test_that("export_doi returns all DOI rows with metric columns", {
  local_export_db()
  out <- export_doi()
  expect_equal(nrow(out), 1200)
  expect_s3_class(out$date, "Date")
  expect_true(all(c("control", "object") %in% names(out)))
  expect_false(any(c("batch_c", "batch_o") %in% names(out)))
  expect_type(out$gini, "double")
  expect_type(out$hhi, "double")
  expect_type(out$entropy, "double")
})

test_that("export_doi filters by keyword", {
  local_export_db()
  expect_equal(nrow(export_doi(keyword = "manchester united")), 120)
})

test_that("export_doi treats keyword = NULL as no filter", {
  local_export_db()
  expect_equal(nrow(export_doi(keyword = NULL)), 1200)
})

test_that("export_doi accepts a list of keywords", {
  local_export_db()
  out <- export_doi(keyword = list(c("manchester united", "real madrid")))
  expect_equal(nrow(out), 240)
})

test_that("export_doi validates the keyword argument", {
  local_export_db()
  test_keyword(fun = export_doi)
})

test_that("export_doi validates object and accepts a vector filter", {
  local_export_db()
  test_object(fun = export_doi)
  expect_equal(nrow(export_doi(object = 1:5)), 1200)
})

test_that("export_doi validates control and accepts a vector filter", {
  local_export_db()
  test_control(fun = export_doi)
  expect_equal(nrow(export_doi(control = 1:5)), 1200)
})

test_that("export_doi validates locations and accepts a vector filter", {
  local_export_db()
  test_locations(fun = export_doi)
  expect_equal(nrow(export_doi(locations = c("countries", "us_states"))), 1200)
})

# empty results ------------------------------------------------------------------
test_that("export functions return zero-row frames for filters that match nothing", {
  local_export_db()

  expect_equal(nrow(export_control(control = 99)), 0)
  expect_equal(nrow(export_control(location = "XX")), 0)
  expect_equal(nrow(export_object(keyword = "no such keyword")), 0)
  expect_equal(nrow(export_score(object = 99)), 0)
  expect_equal(nrow(export_voi(keyword = "no such keyword")), 0)
  expect_equal(nrow(export_doi(locations = "us_states")), 0)
})

test_that("export functions keep their column structure on empty results", {
  local_export_db()

  out <- export_score(object = 99)
  expect_named(out, c("location", "keyword", "date", "score", "control", "object"))
  out <- export_doi(control = 99)
  expect_named(
    out,
    c("keyword", "date", "gini", "hhi", "entropy", "control", "object", "locations")
  )
})

test_that("keyword filter takes precedence over object filter", {
  local_export_db()

  # object = 99 matches nothing, but keyword is provided and wins.
  out <- export_object(keyword = "manchester united", object = 99)
  expect_equal(nrow(out), 240)
})
