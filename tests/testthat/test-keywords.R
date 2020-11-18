# setup ----
library(dplyr)

setwd(tempdir())
initialize_db()
start_db()

# add control keywords - vector ----
test_that("keywords_control1", {
  expect_message(
    add_control_keyword(
      keyword = c("gmail", "maps", "translate", "wikipedia", "youtube"),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "control") %>%
    collect() %>%
    count(batch)
  out_time <- filter(.tbl_time, batch == 1 & type == "control") %>%
    collect() %>%
    count(batch)
  expect_equal(out_keywords$n[[1]], 5)
  expect_equal(out_time$n[[1]], 1)
})

# add control keywords - long vector ----
test_that("keywords_control2", {
  expect_message(
    new_batch <- add_control_keyword(
      keyword = c("gmail", "maps", "news", "translate", "weather", "wikipedia", "youtube"),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch > 1 & type == "control") %>%
    collect() %>%
    count(batch)
  out_time <- filter(.tbl_time, batch > 1 & type == "control") %>%
    collect() %>%
    count(batch)
  expect_equal(nrow(out_keywords), 2)
  expect_equal(out_keywords$n[[1]], 5)
  expect_equal(out_keywords$n[[2]], 2)
  expect_equal(nrow(out_time), 2)
  expect_equal(out_time$n[[1]], 1)
  expect_equal(out_time$n[[2]], 1)
})

# add control keywords - list ----
test_that("keywords_control3", {
  expect_message(
    new_batch <- add_control_keyword(
      keyword = list(
        c("gmail", "maps", "news"),
        c("translate", "weather", "wikipedia", "youtube")
      ),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch > 3 & type == "control") %>%
    collect() %>%
    count(batch)
  out_time <- filter(.tbl_time, batch > 3 & type == "control") %>%
    collect() %>%
    count(batch)
  expect_equal(nrow(out_keywords), 2)
  expect_equal(out_keywords$n[[1]], 3)
  expect_equal(out_keywords$n[[2]], 4)
  expect_equal(nrow(out_time), 2)
  expect_equal(out_time$n[[1]], 1)
  expect_equal(out_time$n[[2]], 1)
})

# add object keywords - vector ----
test_that("keywords_object1", {
  expect_message(
    add_object_keyword(
      keyword = c("apple", "facebook", "google", "microsoft"),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch == 1 & type == "object") %>%
    collect() %>%
    count(batch)
  out_time <- filter(.tbl_time, batch == 1 & type == "object") %>%
    collect() %>%
    count(batch)
  expect_equal(out_keywords$n[[1]], 4)
  expect_equal(out_time$n[[1]], 1)
})

# add object keywords - long vector ----
test_that("keywords_object2", {
  expect_message(
    new_batch <- add_object_keyword(
      keyword = c("amazon", "apple", "facebook", "google", "microsoft", "netflix", "twitter"),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch > 1 & type == "object") %>%
    collect() %>%
    count(batch)
  out_time <- filter(.tbl_time, batch > 1 & type == "object") %>%
    collect() %>%
    count(batch)
  expect_equal(nrow(out_keywords), 2)
  expect_equal(out_keywords$n[[1]], 4)
  expect_equal(out_keywords$n[[2]], 3)
  expect_equal(nrow(out_time), 2)
  expect_equal(out_time$n[[1]], 1)
  expect_equal(out_time$n[[2]], 1)
})

# add object keywords - list ----
test_that("keywords_object3", {
  expect_message(
    new_batch <- add_object_keyword(
      keyword = list(
        c("amazon", "apple", "facebook", "google"),
        c("microsoft", "netflix", "twitter")
      ),
      time = "2010-01-01 2019-12-31"
    )
  )
  out_keywords <- filter(.tbl_keywords, batch > 3 & type == "object") %>%
    collect() %>%
    count(batch)
  out_time <- filter(.tbl_time, batch > 3 & type == "object") %>%
    collect() %>%
    count(batch)
  expect_equal(nrow(out_keywords), 2)
  expect_equal(out_keywords$n[[1]], 4)
  expect_equal(out_keywords$n[[2]], 3)
  expect_equal(nrow(out_time), 2)
  expect_equal(out_time$n[[1]], 1)
  expect_equal(out_time$n[[2]], 1)
})

# disconnect ----
disconnect_db()
unlink("db", recursive = TRUE)
