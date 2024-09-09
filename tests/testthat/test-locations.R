# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

location_set <- c("AT", "DE", "CH", "CN", "JP", "US")

# add location set -------------------------------------------------------------
test_that("add_loc1", {
  expect_message(
    add_locations(
      locations = location_set[1:3],
      type = "dach",
      export = FALSE
    ),
    "Successfully created new location set dach \\(AT, DE, CH\\)\\."
  )

  expect_equal(
    gt.env$dach,
    NULL
  )

  expect_message(
    add_locations(
      locations = location_set[4:5],
      type = "asia",
      export = TRUE
    ),
    "Successfully created new location set asia \\(CN, JP\\)\\."
  )

  expect_identical(
    gt.env$asia,
    location_set[4:5]
  )
})

disconnect_db()
start_db()

test_that("add_loc2", {
  expect_identical(
    gt.env$dach,
    location_set[1:3]
  )

  expect_identical(
    gt.env$asia,
    location_set[4:5]
  )
})

# enter data -------------------------------------------------------------------
add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  start_date = "2010-01",
  end_date = "2019-12"
)

add_object_keyword(
  keyword = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
  start_date = "2010-01",
  end_date = "2019-12"
)

dbAppendTable(gt.env$globaltrends_db, "data_control", example_control)
dbAppendTable(gt.env$globaltrends_db, "data_object", example_object)

# compute score ----------------------------------------------------------------
test_that("compute_score1", {
  out <- capture_messages(compute_score(object = 1, control = 1, locations = gt.env$asia))

  expect_match(
    out,
    "Successfully computed search score | control: 1 | object: 1 | location: CN [1/2]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully computed search score | control: 1 | object: 1 | location: JP [2/2]",
    all = FALSE
  )

  out <- gt.env$tbl_score %>%
    count(location) %>%
    collect()

  expect_identical(
    out$location,
    location_set[4:5]
  )

  expect_equal(
    out$n,
    c(480, 480)
  )
})

test_that("compute_score2", {
  compute_score(object = 1, control = 1, locations = gt.env$countries)

  out <- gt.env$tbl_score %>%
    count(location) %>%
    collect()

  expect_identical(
    out$location,
    location_set[4:6]
  )

  expect_equal(
    out$n,
    c(480, 480, 480)
  )
})

# compute doi ------------------------------------------------------------------
test_that("compute_doi", {
  compute_doi(object = 1, control = 1, locations = "countries")

  expect_message(
    compute_doi(object = 1, control = 1, locations = "asia"),
    "Successfully computed DOI | control: 1 | object: 1 [1/1]"
  )

  out <- gt.env$tbl_doi %>%
    count(locations) %>%
    collect()

  expect_identical(
    out$locations,
    c("asia", "countries")
  )

  expect_equal(
    out$n,
    c(1440, 1440)
  )

  out <- gt.env$tbl_doi %>%
    group_by(locations) %>%
    summarise(
      gini = mean(gini, na.rm = TRUE),
      hhi = mean(hhi, na.rm = TRUE),
      entropy = mean(entropy, na.rm = TRUE)
    ) %>%
    collect()

  expect_false(all(out$gini[out$locations == "asia"] == out$gini[out$locations == "countries"]))
  expect_false(all(out$hhi[out$locations == "asia"] == out$hhi[out$locations == "countries"]))
  expect_false(all(out$entropy[out$locations == "asia"] == out$entropy[out$locations == "countries"]))
})

# export score -----------------------------------------------------------------
test_that("export_score", {
  out1 <- export_score(location = gt.env$asia)
  out2 <- export_score(location = gt.env$countries)

  expect_equal(
    unique(out1$location),
    location_set[4:5]
  )

  expect_equal(
    unique(out2$location),
    location_set[4:6]
  )
})

# export doi -------------------------------------------------------------------
test_that("export_doi", {
  out1 <- export_doi(locations = "asia")
  out2 <- export_doi(locations = "countries")

  expect_false(all(out1$gini == out2$gini))
  expect_false(all(out1$hhi == out2$hhi))
  expect_false(all(out1$entropy == out2$entropy))
})

# plot -------------------------------------------------------------------------
test_that("plot_ts", {
  out <- export_doi(object = 1)
  plot1 <- plot_ts(out, locations = "asia")
  plot2 <- plot_ts(out, locations = "countries")

  expect_false(identical(plot1, plot2))
})

test_that("plot_box", {
  out <- export_doi(object = 1)
  plot1 <- plot_box(out, locations = "asia")
  plot2 <- plot_box(out, locations = "countries")

  expect_false(identical(plot1, plot2))
})

# namibia ----------------------------------------------------------------------
test_that("namibia1", {
  expect_warning(
    add_locations(c("NA", "AT"), "test"),
    "Unfortunately, the Google Trends API cannot handle the location 'NA - Namibia'. The location 'NA' has been dropped."
  )
})

test_that("namibia2", {
  expect_error(
    add_locations("NA", "test"),
    "Unfortunately, the Google Trends API cannot handle the location 'NA - Namibia'. The location 'NA' has been dropped.\nThe argument 'locations' now has lenght 0!"
  )
})

# signals ----------------------------------------------------------------------
test_that("signals1", {
  test_locations(fun = add_locations, type = "A")
})

test_that("signals3", {
  expect_error(
    add_locations(locations = "A", type = "A", export = 1),
    "'export' must be object of type logical.\nYou provided an object of type double."
  )
  expect_error(
    add_locations(locations = "A", type = "A", export = "A"),
    "'export' must be object of type logical.\nYou provided an object of type character."
  )
  expect_error(
    add_locations(locations = "A", type = "A", export = sum),
    "'export' must be object of type logical.\nYou provided an object of type builtin."
  )
  expect_error(
    add_locations(locations = "A", type = "A", export = c(TRUE, TRUE)),
    "'export' must be object of length 1.\nYou provided an object of length 2."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
