# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# add location set -------------------------------------------------------------
test_that("add_loc1", {
  expect_message(
    add_locations(
      locations = c("AT", "DE", "CH"),
      type = "dach",
      export = FALSE
    ),
    "Successfully created new location set dach \\(AT, DE, CH\\)\\."
  )

  expect_error(
    dach,
    "object 'dach' not found"
  )

  expect_message(
    add_locations(
      locations = c("CN", "JP"),
      type = "asia",
      export = TRUE
    ),
    "Successfully created new location set asia \\(CN, JP\\)\\."
  )

  expect_identical(
    asia,
    c("CN", "JP")
  )
})

disconnect_db()
start_db()

test_that("add_loc2", {
  expect_identical(
    dach,
    c("AT", "DE", "CH")
  )

  expect_identical(
    asia,
    c("CN", "JP")
  )
})

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_control", data_control, append = TRUE)
dbWriteTable(globaltrends_db, "data_object", data_object, append = TRUE)

# compute score ----------------------------------------------------------------
test_that("compute_score1", {
  out <- capture_messages(compute_score(object = 1, control = 1, locations = asia))

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

  out <- tbl_score %>%
    count(location) %>%
    collect()

  expect_identical(
    out$location,
    c("CN", "JP")
  )

  expect_equal(
    out$n,
    c(480, 480)
  )
})

test_that("compute_score2", {
  compute_score(object = 1, control = 1, locations = countries)

  out <- tbl_score %>%
    count(location) %>%
    collect()

  expect_identical(
    out$location,
    c("CN", "JP", "US")
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

  out <- tbl_doi %>%
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

  out <- tbl_doi %>%
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
  out1 <- export_score(locations = asia)
  out2 <- export_score(locations = countries)

  expect_equal(
    unique(out1$location),
    c("CN", "JP")
  )

  expect_equal(
    unique(out2$location),
    c("CN", "JP", "US")
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

# plot doi ts ------------------------------------------------------------------
test_that("plot_doi_ts", {
  out <- export_doi(object = 1)
  plot1 <- plot_doi_ts(out, locations = "asia")
  plot2 <- plot_doi_ts(out, locations = "countries")

  expect_false(identical(plot1, plot2))
})

# plot doi box -----------------------------------------------------------------
test_that("plot_doi_box", {
  out <- export_doi(object = 1)
  plot1 <- plot_doi_box(out, locations = "asia")
  plot2 <- plot_doi_box(out, locations = "countries")

  expect_false(identical(plot1, plot2))
})

# signals ----------------------------------------------------------------------
test_that("signals1", {
  expect_error(
    add_locations(locations = 1, type = "A"),
    "'locations' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_locations(locations = TRUE, type = "A"),
    "'locations' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_locations(locations = sum, type = "A"),
    "'locations' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    add_locations(locations = as.list(letters[1:3]), type = "A"),
    "'locations' must be object of type character.\nYou provided an object of type list."
  )
})

test_that("signals2", {
  expect_error(
    add_locations(type = 1, locations = "A"),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_locations(type = TRUE, locations = "A"),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_locations(type = sum, locations = "A"),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    add_locations(type = letters[1:3], locations = "A"),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
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
