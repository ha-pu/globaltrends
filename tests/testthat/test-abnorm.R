# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
initialize_db()
start_db()

# enter data -------------------------------------------------------------------
data <- filter(data_control, batch == 1 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(data_object, batch_c == 1 & batch_o %in% 1:3 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)
data <- filter(data_score, batch_c == 1 & batch_o %in% 1:3 & location %in% c(countries[1:2], "world"))
dbWriteTable(globaltrends_db, "data_score", data, append = TRUE)
data <- filter(data_doi, batch_c == 1 & batch_o %in% 1:3 & locations == "countries")
dbWriteTable(globaltrends_db, "data_doi", data, append = TRUE)

# compute_abnorm.exp_score -----------------------------------------------------
data <- export_score(object = 1)
test_that("exp_score1", {
  out <- compute_abnorm(data)
  expect_s3_class(out, "abnorm_score")
  expect_equal(nrow(out), 960)
  out <- na.omit(out)
  expect_equal(nrow(out), 864)
})

data <- export_score(keyword = "fc barcelona")
test_that("exp_score2", {
  out <- compute_abnorm(data)
  expect_equal(nrow(out), 240)
  out <- na.omit(out)
  expect_equal(nrow(out), 216)
})

test_that("exp_score3", {
  expect_error(
    compute_abnorm(data, train_win = "A"),
    "'train_win' must be object of type numeric.\nYou provided an object of type character."
  )
  expect_error(
    compute_abnorm(data, train_win = TRUE),
    "'train_win' must be object of type numeric.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, train_win = sum),
    "'train_win' must be object of type numeric.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, train_win = 1:5),
    "'train_win' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("exp_score4", {
  expect_error(
    compute_abnorm(data, train_break = "A"),
    "'train_break' must be object of type numeric.\nYou provided an object of type character."
  )
  expect_error(
    compute_abnorm(data, train_break = TRUE),
    "'train_break' must be object of type numeric.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, train_break = sum),
    "'train_break' must be object of type numeric.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, train_break = 1:5),
    "'train_break' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("exp_score5", {
  expect_error(
    compute_abnorm(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_abnorm(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    compute_abnorm(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("exp_score6", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, train_win = 12)
  out3 <- compute_abnorm(data, train_win = 10)
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_score7", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, train_break = 0)
  out3 <- compute_abnorm(data, train_break = 1)
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_score8", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, type = "obs")
  out3 <- compute_abnorm(data, type = "sad")
  out4 <- compute_abnorm(data, type = "trd")
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# compute_abnorm.exp_voi -------------------------------------------------------
data <- export_voi(object = 1)
test_that("exp_voi1", {
  out <- compute_abnorm(data)
  expect_s3_class(out, "abnorm_voi")
  expect_equal(nrow(out), 480)
  out <- na.omit(out)
  expect_equal(nrow(out), 432)
})

data <- export_voi(keyword = "fc barcelona")
test_that("exp_voi2", {
  out <- compute_abnorm(data)
  expect_equal(nrow(out), 120)
  out <- na.omit(out)
  expect_equal(nrow(out), 108)
})

test_that("exp_voi3", {
  expect_error(
    compute_abnorm(data, train_win = "A"),
    "'train_win' must be object of type numeric.\nYou provided an object of type character."
  )
  expect_error(
    compute_abnorm(data, train_win = TRUE),
    "'train_win' must be object of type numeric.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, train_win = sum),
    "'train_win' must be object of type numeric.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, train_win = 1:5),
    "'train_win' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("exp_voi4", {
  expect_error(
    compute_abnorm(data, train_break = "A"),
    "'train_break' must be object of type numeric.\nYou provided an object of type character."
  )
  expect_error(
    compute_abnorm(data, train_break = TRUE),
    "'train_break' must be object of type numeric.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, train_break = sum),
    "'train_break' must be object of type numeric.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, train_break = 1:5),
    "'train_break' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("exp_voi5", {
  expect_error(
    compute_abnorm(data, type = 1),
    "'type' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_abnorm(data, type = "A"),
    "'type' must be either 'obs', 'sad', or 'trd'.\nYou provided A."
  )
  expect_error(
    compute_abnorm(data, type = TRUE),
    "'type' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, type = sum),
    "'type' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, type = c("obs", "sad", "trd")),
    "'type' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("exp_voi6", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, train_win = 12)
  out3 <- compute_abnorm(data, train_win = 10)
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_voi7", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, train_break = 0)
  out3 <- compute_abnorm(data, train_break = 1)
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_voi8", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, type = "obs")
  out3 <- compute_abnorm(data, type = "sad")
  out4 <- compute_abnorm(data, type = "trd")
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# compute_abnorm.exp_doi -------------------------------------------------------
data <- export_doi(object = 1)
test_that("exp_doi1", {
  out <- compute_abnorm(data)
  expect_s3_class(out, "abnorm_doi")
  expect_equal(nrow(out), 1440)
  out <- na.omit(out)
  expect_equal(nrow(out), 1296)
})

data <- export_doi(keyword = "fc barcelona")
test_that("exp_doi2", {
  out <- compute_abnorm(data)
  expect_equal(nrow(out), 360)
  out <- na.omit(out)
  expect_equal(nrow(out), 324)
})

test_that("exp_doi3", {
  expect_error(
    compute_abnorm(data, train_win = "A"),
    "'train_win' must be object of type numeric.\nYou provided an object of type character."
  )
  expect_error(
    compute_abnorm(data, train_win = TRUE),
    "'train_win' must be object of type numeric.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, train_win = sum),
    "'train_win' must be object of type numeric.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, train_win = 1:5),
    "'train_win' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("exp_doi4", {
  expect_error(
    compute_abnorm(data, train_break = "A"),
    "'train_break' must be object of type numeric.\nYou provided an object of type character."
  )
  expect_error(
    compute_abnorm(data, train_break = TRUE),
    "'train_break' must be object of type numeric.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, train_break = sum),
    "'train_break' must be object of type numeric.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, train_break = 1:5),
    "'train_break' must be object of length 1.\nYou provided an object of length 5."
  )
})

test_that("exp_doi5", {
  expect_error(
    compute_abnorm(data, measure = 1),
    "'measure' must be object of type character.\nYou provided an object of type double."
  )
  expect_error(
    compute_abnorm(data, measure = "A"),
    "'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou provided A."
  )
  expect_error(
    compute_abnorm(data, measure = TRUE),
    "'measure' must be object of type character.\nYou provided an object of type logical."
  )
  expect_error(
    compute_abnorm(data, measure = sum),
    "'measure' must be object of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    compute_abnorm(data, measure = c("gini", "hhi", "entropy")),
    "'measure' must be object of length 1.\nYou provided an object of length 3."
  )
})

test_that("exp_doi6", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, train_win = 12)
  out3 <- compute_abnorm(data, train_win = 10)
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_doi7", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, train_break = 0)
  out3 <- compute_abnorm(data, train_break = 1)
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
})

test_that("exp_doi8", {
  out1 <- compute_abnorm(data)
  out2 <- compute_abnorm(data, measure = "gini")
  out3 <- compute_abnorm(data, measure = "hhi")
  out4 <- compute_abnorm(data, measure = "entropy")
  
  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# compute_abnorm methods -------------------------------------------------------
test_that("methods", {
  data <- export_control(control = 1)
  expect_error(
    compute_abnorm(data),
    "no applicable method"
  )
  data <- export_object(object = 1)
  expect_error(
    compute_abnorm(data),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")