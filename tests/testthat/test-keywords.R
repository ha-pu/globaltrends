# setup ------------------------------------------------------------------------
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
source("../test_functions.r")

initialize_db()
start_db()

# add control keywords - vector ------------------------------------------------
test_that("keywords_control1", {
  expect_message(
    new_batch <- add_control_keyword(
      keyword = c("gmail", "maps", "translate", "wikipedia", "youtube"),
      start_date = "2010-01",
      end_date = "2019-12"
    ),
    "Successfully created new control batch 1 \\(gmail, maps, translate, wikipedia, youtube, 2010-01-2019-12\\)\\."
  )
  out <- filter(
    gt.env$tbl_keywords,
    batch == 1 & type == "control"
  )
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(out$n, 5)

  out <- filter(gt.env$tbl_time, batch == 1 & type == "control")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(out$n, 1)
})

# add control keywords - long vector -------------------------------------------
test_that("keywords_control2", {
  out <- capture_messages(
    new_batch <- add_control_keyword(
      keyword = c(
        "gmail",
        "maps",
        "news",
        "translate",
        "weather",
        "wikipedia",
        "youtube"
      ),
      start_date = "2010-01",
      end_date = "2019-12"
    )
  )
  expect_match(
    out,
    "Successfully created new control batch 2 \\(gmail, maps, news, translate, weather, 2010-01-2019-12\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new control batch 3 \\(wikipedia, youtube, 2010-01-2019-12\\)\\.",
    all = FALSE
  )

  out <- filter(gt.env$tbl_keywords, batch > 1 & type == "control")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 5)
  expect_equal(out$n[[2]], 2)

  out <- filter(gt.env$tbl_time, batch > 1 & type == "control")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 1)
  expect_equal(out$n[[2]], 1)
})

# add control keywords - list --------------------------------------------------
test_that("keywords_control3", {
  out <- capture_messages(
    new_batch <- add_control_keyword(
      keyword = list(
        c("gmail", "maps", "news"),
        c("translate", "weather", "wikipedia", "youtube")
      ),
      start_date = "2010-01",
      end_date = "2019-12"
    )
  )
  expect_match(
    out,
    "Successfully created new control batch 4 \\(gmail, maps, news, 2010-01-2019-12\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new control batch 5 \\(translate, weather, wikipedia, youtube, 2010-01-2019-12\\)\\.",
    all = FALSE
  )

  out <- filter(gt.env$tbl_keywords, batch > 3 & type == "control")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 3)
  expect_equal(out$n[[2]], 4)

  out <- filter(gt.env$tbl_time, batch > 3 & type == "control")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 1)
  expect_equal(out$n[[2]], 1)
})

# add object keywords - vector -------------------------------------------------
test_that("keywords_object1", {
  expect_message(
    new_batch <- add_object_keyword(
      keyword = c("apple", "facebook", "google", "microsoft"),
      start_date = "2010-01",
      end_date = "2019-12"
    ),
    "Successfully created new object batch 1 \\(apple, facebook, google, microsoft, 2010-01-2019-12\\)\\."
  )

  out <- filter(gt.env$tbl_keywords, batch == 1 & type == "object")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(out$n, 4)

  out <- filter(gt.env$tbl_time, batch == 1 & type == "object")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(out$n, 1)
})

# add object keywords - long vector --------------------------------------------
test_that("keywords_object2", {
  out <- capture_messages(
    new_batch <- add_object_keyword(
      keyword = c(
        "amazon",
        "apple",
        "facebook",
        "google",
        "microsoft",
        "netflix",
        "twitter"
      ),
      start_date = "2010-01",
      end_date = "2019-12"
    )
  )
  expect_match(
    out,
    "Successfully created new object batch 2 \\(amazon, apple, facebook, google, 2010-01-2019-12\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new object batch 3 \\(microsoft, netflix, twitter, 2010-01-2019-12\\)\\.",
    all = FALSE
  )

  out <- filter(gt.env$tbl_keywords, batch > 1 & type == "object")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 4)
  expect_equal(out$n[[2]], 3)

  out <- filter(gt.env$tbl_time, batch > 1 & type == "object")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 1)
  expect_equal(out$n[[2]], 1)
})

# add object keywords - list ---------------------------------------------------
test_that("keywords_object3", {
  out <- capture_messages(
    new_batch <- add_object_keyword(
      keyword = list(
        c("amazon", "apple", "facebook", "google"),
        c("microsoft", "netflix", "twitter")
      ),
      start_date = "2010-01",
      end_date = "2019-12"
    )
  )
  expect_match(
    out,
    "Successfully created new object batch 4 \\(amazon, apple, facebook, google, 2010-01-2019-12\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new object batch 5 \\(microsoft, netflix, twitter, 2010-01-2019-12\\)\\.",
    all = FALSE
  )

  out <- filter(gt.env$tbl_keywords, batch > 3 & type == "object")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 4)
  expect_equal(out$n[[2]], 3)

  out <- filter(gt.env$tbl_time, batch > 3 & type == "object")
  out <- count(out, batch)
  out <- collect(out)
  expect_equal(length(out$n), 2)
  expect_equal(out$n[[1]], 1)
  expect_equal(out$n[[2]], 1)
})

# add_control / add_keyword signals --------------------------------------------
test_that("add_batch1", {
  test_keyword(fun = add_control_keyword, incl = 4:6)
})

test_that("add_batch2", {
  expect_error(
    add_control_keyword(start_date = 1),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(start_date = TRUE),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(start_date = sum),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(start_date = letters[1:5]),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(end_date = 1),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(end_date = TRUE),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(end_date = sum),
    '"keyword"'
  )
  expect_error(
    add_control_keyword(end_date = letters[1:5]),
    '"keyword"'
  )
})

test_that("add_batch3", {
  test_keyword(fun = add_object_keyword, incl = 4:6)
})

test_that("add_batch4", {
  expect_error(
    add_object_keyword(start_date = 1),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(start_date = TRUE),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(start_date = sum),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(start_date = letters[1:5]),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(end_date = 1),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(end_date = TRUE),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(end_date = sum),
    '"keyword"'
  )
  expect_error(
    add_object_keyword(end_date = letters[1:5]),
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
