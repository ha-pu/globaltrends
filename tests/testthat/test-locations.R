# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

source("../test_functions.r")

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

  expect_equal(
    gt.env$dach,
    NULL
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
    gt.env$asia,
    c("CN", "JP")
  )
})

disconnect_db()
start_db()

test_that("add_loc2", {
  expect_identical(
    gt.env$dach,
    c("AT", "DE", "CH")
  )

  expect_identical(
    gt.env$asia,
    c("CN", "JP")
  )
})

# enter data -------------------------------------------------------------------
add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
  time = "2010-01-01 2019-12-31"
)

dbWriteTable(gt.env$globaltrends_db, "data_control", example_control, append = TRUE)
dbWriteTable(gt.env$globaltrends_db, "data_object", example_object, append = TRUE)

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
    c("CN", "JP")
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
  expect_message(
    add_locations("NA", "test"),
    "Successfully created new location set test \\(NA\\)\\."
  )
  
  out <- gt.env$tbl_locations %>%
    filter(type == "test") %>%
    collect() %>%
    pull(location)
  expect_equal(out, "NX")
})

test_that("namibia2", {
  skip_on_cran()
  skip_if_offline()
  
  add_control_keyword(keyword = "google", time = "2010-01-01 2020-12-31")
  expect_message(
    download_control(control = 2, locations = gt.env$test),
    "Successfully downloaded control data | control: 2 | location: NA [1/1]"
  )
  
  out <- gt.env$tbl_control %>%
    filter(batch == 2) %>%
    collect()
  expect_equal(nrow(out), 132)
  expect_equal(out$location[[1]], "NX")
  
  out <- export_control(location = gt.env$test, control = 2)
  expect_equal(nrow(out), 132)
  expect_equal(out$location[[1]], "NA")
})

test_that("namibia3", {
  skip_on_cran()
  skip_if_offline()
  
  add_object_keyword(keyword = "football", time = "2010-01-01 2020-12-31")
  expect_message(
    download_object(object = 2, control = 2, locations = gt.env$test),
    "Successfully downloaded object data | object: 2 | control: 2 | location: NA [1/1]"
  )
  
  out <- gt.env$tbl_object %>%
    filter(batch_c == 2) %>%
    collect()
  expect_equal(nrow(out), 264)
  expect_equal(out$location[[1]], "NX")
  
  out <- export_object(location = gt.env$test, control = 2)
  expect_equal(nrow(out), 264)
  expect_equal(out$location[[1]], "NA") 
})

test_that("namibia4", {
  skip_on_cran()
  skip_if_offline()
  
  expect_message(
    compute_score(object = 2, control = 2, locations = gt.env$test),
    "Successfully computed search score | control: 2 | object: 2 | location: NA [1/1]"
  )
  
  out <- gt.env$tbl_score %>%
    filter(batch_c == 2) %>%
    collect()
  expect_equal(nrow(out), 132)
  expect_equal(out$location[[1]], "NX")
  
  out <- export_score(location = gt.env$test, control = 2)
  expect_equal(nrow(out), 132)
  expect_equal(out$location[[1]], "NA") 
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
