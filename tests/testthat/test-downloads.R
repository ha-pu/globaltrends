# setup ------------------------------------------------------------------------
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

add_control_keyword(
  keyword = c("gmail", "map", "translate", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
  time = "2010-01-01 2019-12-31"
)

# try download object ----------------------------------------------------------
test_that("download_object1", {
  out <- expect_message(
    download_object(object = 1, locations = gt.env$countries[[1]]),
    "Download for object data failed.\nThere is no data in 'data_control' for control batch 1 and location US."
  )
})

# download control --------------------------------------------------------------
test_that("download_control1", {
  skip_on_cran()
  skip_if_offline()
  
  out <- capture_messages(download_control(control = 1, locations = gt.env$countries[1:3]))

  expect_match(
    out,
    "Successfully downloaded control data | control: 1 | location: US [1/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded control data | control: 1 | location: CN [2/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded control data | control: 1 | location: JP [3/3]",
    all = FALSE
  )

  out <- filter(gt.env$tbl_control, batch == 1 & location != "world")
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

# re-download control ----------------------------------------------------------
test_that("download_control2", {
  skip_on_cran()
  skip_if_offline()
  
  expect_message(
    download_control(control = 1, locations = gt.env$countries[[1]]),
    "Control data already available | control: 1 | location: US [1/1]"
  )
})

# download control global ------------------------------------------------------
test_that("download_control3", {
  skip_on_cran()
  skip_if_offline()
  
  expect_message(
    download_control_global(control = 1),
    "Successfully downloaded control data | control: 1 | location: world [1/1]"
  )
  out <- filter(gt.env$tbl_control, batch == 1 & location == "world")
  out <- collect(out)
  expect_equal(nrow(out), 600)
})

# download control signals -----------------------------------------------------
test_that("download_control4", {
  test_control(fun = download_control, incl = c(1, 6:8))
})

test_that("download_control5", {
  expect_error(
    download_control(locations = 1),
    '"control"'
  )
  expect_error(
    download_control(locations = TRUE),
    '"control"'
  )
  expect_error(
    download_control(locations = sum),
    '"control"'
  )
})

test_that("download_control6", {
  test_control(fun = download_control_global, incl = c(1, 6:8))
})

# download object --------------------------------------------------------------
test_that("download_object2", {
  skip_on_cran()
  skip_if_offline()
  
  out <- capture_messages(download_object(object = 1, locations = gt.env$countries[1:3]))

  expect_match(
    out,
    "Successfully downloaded object data | object: 1 | control: 1 | location: US [1/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded object data | object: 1 | control: 1 | location: CN [2/3]",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully downloaded object data | object: 1 | control: 1 | location: JP [3/3]",
    all = FALSE
  )

  out <- filter(gt.env$tbl_object, batch_o == 1 & location != "world")
  out <- collect(out)
  expect_equal(nrow(out), 1800)
})

# re-download object -----------------------------------------------------------
test_that("download_object3", {
  skip_on_cran()
  skip_if_offline()
  
  expect_message(
    download_object(object = 1, locations = gt.env$countries[[1]]),
    "Object data already available | object: 1 | control: 1 | location: US [1/1]"
  )
})

# download object global -------------------------------------------------------
test_that("download_object4", {
  skip_on_cran()
  skip_if_offline()
  
  expect_message(
    download_object_global(object = 1),
    "Successfully downloaded object data | object: 1 | control: 1 | location: world [1/1]"
  )
  out <- filter(gt.env$tbl_object, batch_o == 1 & location == "world")
  out <- collect(out)
  expect_equal(nrow(out), 600)
})

# download object signals ------------------------------------------------------
test_that("download_object5", {
  test_object(fun = download_object, incl = c(1, 6:8))
})

test_that("download_object6", {
  test_control(fun = download_object, incl = 1:5, object = 1)
})

test_that("download_object7", {
  test_locations(fun = download_object, object = 1)
})

test_that("download_object8", {
  test_object(fun = download_object_global, incl = c(1, 6:8))
})

test_that("download_object9", {
  test_control(fun = download_object_global, incl = 1:5, object = 1)
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
