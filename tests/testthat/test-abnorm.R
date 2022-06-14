# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

source("../test_functions.r")

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
data <- filter(example_control, batch == 1 & location %in% c(gt.env$countries[1:2], "world"))
dbWriteTable(gt.env$globaltrends_db, "data_control", data, append = TRUE)
data <- filter(example_object, batch_c == 1 & batch_o %in% 1:3 & location %in% c(gt.env$countries[1:2], "world"))
dbWriteTable(gt.env$globaltrends_db, "data_object", data, append = TRUE)
data <- filter(example_score, batch_c == 1 & batch_o %in% 1:3 & location %in% c(gt.env$countries[1:2], "world"))
dbWriteTable(gt.env$globaltrends_db, "data_score", data, append = TRUE)
data <- filter(example_doi, batch_c == 1 & batch_o %in% 1:3 & locations == "countries")
dbWriteTable(gt.env$globaltrends_db, "data_doi", data, append = TRUE)

# get_abnorm_hist.exp_score ---------------------------------------------------
data <- export_score(object = 1)
test_that("exp_score1", {
  out <- get_abnorm_hist(data)
  expect_s3_class(out, "abnorm_score")
  expect_equal(nrow(out), 960)
  out <- na.omit(out)
  expect_equal(nrow(out), 864)
})

data <- export_score(keyword = "fc barcelona")
test_that("exp_score2", {
  out <- get_abnorm_hist(data)
  expect_equal(nrow(out), 240)
  out <- na.omit(out)
  expect_equal(nrow(out), 216)
})

test_that("exp_score3", {
  test_train(fun = get_abnorm_hist, data = data)
})

test_that("exp_score5", {
  test_type(fun = get_abnorm_hist, data = data)
})

test_that("exp_score6", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, train_win = 12)
  out3 <- get_abnorm_hist(data, train_win = 10)

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_score7", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, train_break = 0)
  out3 <- get_abnorm_hist(data, train_break = 1)

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_score8", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, type = "obs")
  out3 <- get_abnorm_hist(data, type = "sad")
  out4 <- get_abnorm_hist(data, type = "trd")

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# get_abnorm_hist.exp_voi -----------------------------------------------------
data <- export_voi(object = 1)
test_that("exp_voi1", {
  out <- get_abnorm_hist(data)
  expect_s3_class(out, "abnorm_voi")
  expect_equal(nrow(out), 480)
  out <- na.omit(out)
  expect_equal(nrow(out), 432)
})

data <- export_voi(keyword = "fc barcelona")
test_that("exp_voi2", {
  out <- get_abnorm_hist(data)
  expect_equal(nrow(out), 120)
  out <- na.omit(out)
  expect_equal(nrow(out), 108)
})

test_that("exp_voi3", {
  test_train(fun = get_abnorm_hist, data = data)
})

test_that("exp_voi5", {
  test_type(fun = get_abnorm_hist, data = export_score(keyword = "fc barcelona"))
})

test_that("exp_voi6", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, train_win = 12)
  out3 <- get_abnorm_hist(data, train_win = 10)

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_voi7", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, train_break = 0)
  out3 <- get_abnorm_hist(data, train_break = 1)

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_voi8", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, type = "obs")
  out3 <- get_abnorm_hist(data, type = "sad")
  out4 <- get_abnorm_hist(data, type = "trd")

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# get_abnorm_hist.exp_doi -----------------------------------------------------
data <- export_doi(object = 1)
test_that("exp_doi1", {
  out <- get_abnorm_hist(data)
  expect_s3_class(out, "abnorm_doi")
  expect_equal(nrow(out), 1440)
  out <- na.omit(out)
  expect_equal(nrow(out), 1296)
})

data <- export_doi(keyword = "fc barcelona")
test_that("exp_doi2", {
  out <- get_abnorm_hist(data)
  expect_equal(nrow(out), 360)
  out <- na.omit(out)
  expect_equal(nrow(out), 324)
})

test_that("exp_doi3", {
  test_train(fun = get_abnorm_hist, data = data)
})

test_that("exp_doi5", {
  test_measure(fun = get_abnorm_hist, data = data)
})

test_that("exp_doi6", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, train_win = 12)
  out3 <- get_abnorm_hist(data, train_win = 10)

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_doi7", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, train_break = 0)
  out3 <- get_abnorm_hist(data, train_break = 1)

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_doi8", {
  out1 <- get_abnorm_hist(data)
  out2 <- get_abnorm_hist(data, measure = "gini")
  out3 <- get_abnorm_hist(data, measure = "hhi")
  out4 <- get_abnorm_hist(data, measure = "entropy")

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# get_abnorm_hist methods -----------------------------------------------------
test_that("get_abnorm_hist.methods", {
  data <- export_control(control = 1)
  expect_error(
    get_abnorm_hist(data),
    "no applicable method"
  )
  data <- export_object(object = 1)
  expect_error(
    get_abnorm_hist(data),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
