# setup ------------------------------------------------------------------------
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")

initialize_db()
start_db()

# add control keywords - vector ------------------------------------------------
test_that("keywords_control1", {
  expect_message(
    add_control_keyword(
      keyword = c("gmail", "maps", "translate", "wikipedia", "youtube"),
      time = "2010-01-01 2019-12-31"
    ),
    "Successfully created new control batch 1 \\(gmail, maps, translate, wikipedia, youtube, 2010-01-01 2019-12-31\\)\\."
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

# add control keywords - long vector -------------------------------------------
test_that("keywords_control2", {
  out <- capture_messages(
    new_batch <- add_control_keyword(
      keyword = c("gmail", "maps", "news", "translate", "weather", "wikipedia", "youtube"),
      time = "2010-01-01 2019-12-31"
    )
  )
  expect_match(
    out,
    "Successfully created new control batch 2 \\(gmail, maps, news, translate, weather, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new control batch 3 \\(wikipedia, youtube, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
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

# add control keywords - list --------------------------------------------------
test_that("keywords_control3", {
  out <- capture_messages(
    new_batch <- add_control_keyword(
      keyword = list(
        c("gmail", "maps", "news"),
        c("translate", "weather", "wikipedia", "youtube")
      ),
      time = "2010-01-01 2019-12-31"
    )
  )
  expect_match(
    out,
    "Successfully created new control batch 4 \\(gmail, maps, news, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new control batch 5 \\(translate, weather, wikipedia, youtube, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
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

# add object keywords - vector -------------------------------------------------
test_that("keywords_object1", {
  expect_message(
    add_object_keyword(
      keyword = c("apple", "facebook", "google", "microsoft"),
      time = "2010-01-01 2019-12-31"
    ),
    "Successfully created new object batch 1 \\(apple, facebook, google, microsoft, 2010-01-01 2019-12-31\\)\\."
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

# add object keywords - long vector --------------------------------------------
test_that("keywords_object2", {
  out <- capture_messages(
    new_batch <- add_object_keyword(
      keyword = c("amazon", "apple", "facebook", "google", "microsoft", "netflix", "twitter"),
      time = "2010-01-01 2019-12-31"
    )
  )
  expect_match(
    out,
    "Successfully created new object batch 2 \\(amazon, apple, facebook, google, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new object batch 3 \\(microsoft, netflix, twitter, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
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

# add object keywords - list ---------------------------------------------------
test_that("keywords_object3", {
  out <- capture_messages(
    new_batch <- add_object_keyword(
      keyword = list(
        c("amazon", "apple", "facebook", "google"),
        c("microsoft", "netflix", "twitter")
      ),
      time = "2010-01-01 2019-12-31"
    )
  )
  expect_match(
    out,
    "Successfully created new object batch 4 \\(amazon, apple, facebook, google, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new object batch 5 \\(microsoft, netflix, twitter, 2010-01-01 2019-12-31\\)\\.",
    all = FALSE
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

# add_control / add_keyword signals --------------------------------------------
test_that("add_batch1", {
  expect_error(
    add_control_keyword(keyword = 1),
    "no applicable method"
  )
  expect_error(
    add_control_keyword(keyword = TRUE),
    "no applicable method"
  )
  expect_error(
    add_control_keyword(keyword = sum),
    "no applicable method"
  )
})

test_that("add_batch2", {
  expect_error(
    add_control_keyword(time = 1),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(time = TRUE),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(time = sum),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(time = letters[1:5]),
    '"keyword"'
  )
})

test_that("add_batch3", {
  expect_error(
    add_object_keyword(keyword = 1),
    "no applicable method"
  )
  expect_error(
    add_object_keyword(keyword = TRUE),
    "no applicable method"
  )
  expect_error(
    add_object_keyword(keyword = sum),
    "no applicable method"
  )
})

test_that("add_batch4", {
  expect_error(
    add_object_keyword(time = 1),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(time = TRUE),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(time = sum),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(time = letters[1:5]),
    '"keyword"'
  )
})

test_that("add_batch5", {
  expect_error(
    add_control_keyword(keyword = list(letters[1:6])),
    "'keyword' must be object of length 5.\nYou provided an object of length 6."
  )

  expect_error(
    add_object_keyword(keyword = list(letters[1:5])),
    "'keyword' must be object of length 4.\nYou provided an object of length 5."
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
