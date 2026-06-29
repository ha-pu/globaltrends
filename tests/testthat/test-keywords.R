# add control keywords - vector ------------------------------------------------
test_that("keywords_control1", {
  local_db()

  expect_message(
    add_control_keyword(
      keyword = c("gmail", "maps", "translate", "wikipedia", "youtube"),
      start_date = "2010-01",
      end_date = "2019-12"
    ),
    "Successfully created new control batch 1 \\(gmail, maps, translate, wikipedia, youtube, 2010-01-2019-12\\)\\."
  )

  dt <- gt.env$dt_keywords
  out <- nrow(dt[dt$batch == 1L & dt$type == "control", ])
  expect_equal(out, 5L)

  dt <- gt.env$dt_time
  out <- nrow(dt[dt$batch == 1L & dt$type == "control", ])
  expect_equal(out, 1L)
})

# add control keywords - long vector -------------------------------------------
test_that("keywords_control2", {
  local_db()

  out <- capture_messages(
    add_control_keyword(
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
    "Successfully created new control batch 1 \\(gmail, maps, news, translate, weather, 2010-01-2019-12\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new control batch 2 \\(wikipedia, youtube, 2010-01-2019-12\\)\\.",
    all = FALSE
  )

  dt <- gt.env$dt_keywords
  ctrl <- dt[dt$type == "control", ]
  counts <- as.data.frame(table(ctrl$batch))
  expect_equal(nrow(counts), 2L)
  expect_equal(counts$Freq[[1]], 5L)
  expect_equal(counts$Freq[[2]], 2L)

  dt <- gt.env$dt_time
  ctrl <- dt[dt$type == "control", ]
  counts <- as.data.frame(table(ctrl$batch))
  expect_equal(nrow(counts), 2L)
  expect_equal(counts$Freq[[1]], 1L)
  expect_equal(counts$Freq[[2]], 1L)
})

# add object keywords - vector -------------------------------------------------
test_that("keywords_object1", {
  local_db()

  expect_message(
    add_object_keyword(
      keyword = c("apple", "facebook", "google", "microsoft"),
      start_date = "2010-01",
      end_date = "2019-12"
    ),
    "Successfully created new object batch 1 \\(apple, facebook, google, microsoft, 2010-01-2019-12\\)\\."
  )

  dt <- gt.env$dt_keywords
  out <- nrow(dt[dt$batch == 1L & dt$type == "object", ])
  expect_equal(out, 4L)

  dt <- gt.env$dt_time
  out <- nrow(dt[dt$batch == 1L & dt$type == "object", ])
  expect_equal(out, 1L)
})

# add object keywords - long vector --------------------------------------------
test_that("keywords_object2", {
  local_db()

  out <- capture_messages(
    add_object_keyword(
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
    "Successfully created new object batch 1 \\(amazon, apple, facebook, google, 2010-01-2019-12\\)\\.",
    all = FALSE
  )
  expect_match(
    out,
    "Successfully created new object batch 2 \\(microsoft, netflix, twitter, 2010-01-2019-12\\)\\.",
    all = FALSE
  )

  dt <- gt.env$dt_keywords
  obj <- dt[dt$type == "object", ]
  counts <- as.data.frame(table(obj$batch))
  expect_equal(nrow(counts), 2L)
  expect_equal(counts$Freq[[1]], 4L)
  expect_equal(counts$Freq[[2]], 3L)

  dt <- gt.env$dt_time
  obj <- dt[dt$type == "object", ]
  counts <- as.data.frame(table(obj$batch))
  expect_equal(nrow(counts), 2L)
  expect_equal(counts$Freq[[1]], 1L)
  expect_equal(counts$Freq[[2]], 1L)
})

# add_control / add_keyword signals --------------------------------------------
test_that("add_batch1", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(
    add_control_keyword(keyword = sum),
    "cannot coerce type 'builtin' to vector of type 'character'"
  )
  expect_error(
    add_control_keyword(keyword = character(0)),
    "`keyword` must contain at least one term."
  )
})

test_that("add_batch2", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(
    add_control_keyword(start_date = 1),
    "Error: `start_date` must be of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_control_keyword(start_date = TRUE),
    "Error: `start_date` must be of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_control_keyword(start_date = sum),
    "Error: `start_date` must be of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    add_control_keyword(start_date = letters[1:5]),
    "Error: `start_date` must have length <= 1.\nYou provided an object of length 5."
  )
  expect_error(
    add_control_keyword(end_date = 1),
    "Error: `end_date` must be of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_control_keyword(end_date = TRUE),
    "Error: `end_date` must be of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_control_keyword(end_date = sum),
    "Error: `end_date` must be of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    add_control_keyword(end_date = letters[1:5]),
    "Error: `end_date` must have length <= 1.\nYou provided an object of length 5."
  )
})

test_that("add_batch3", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(
    add_object_keyword(keyword = sum),
    "cannot coerce type 'builtin' to vector of type 'character'"
  )
  expect_error(
    add_object_keyword(keyword = character(0)),
    "`keyword` must contain at least one term."
  )
})

test_that("add_batch4", {
  withr::local_envvar(LANGUAGE = "EN")
  expect_error(
    add_object_keyword(start_date = 1),
    "Error: `start_date` must be of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_object_keyword(start_date = TRUE),
    "Error: `start_date` must be of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_object_keyword(start_date = sum),
    "Error: `start_date` must be of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    add_object_keyword(start_date = letters[1:5]),
    "Error: `start_date` must have length <= 1.\nYou provided an object of length 5."
  )
  expect_error(
    add_object_keyword(end_date = 1),
    "Error: `end_date` must be of type character.\nYou provided an object of type double."
  )
  expect_error(
    add_object_keyword(end_date = TRUE),
    "Error: `end_date` must be of type character.\nYou provided an object of type logical."
  )
  expect_error(
    add_object_keyword(end_date = sum),
    "Error: `end_date` must be of type character.\nYou provided an object of type builtin."
  )
  expect_error(
    add_object_keyword(end_date = letters[1:5]),
    "Error: `end_date` must have length <= 1.\nYou provided an object of length 5."
  )
})
