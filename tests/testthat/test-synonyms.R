# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

Sys.setenv("LANGUAGE" = "EN")
source("../test_functions.r")

initialize_db()
start_db()

add_control_keyword(
  keyword = c("gmail", "map", "wikipedia", "youtube"),
  start_date = "2010-01",
  end_date = "2019-12"
)

add_object_keyword(
  keyword = list(
    c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
    c("bayern munich", "bayern munchen")
  ),
  start_date = "2010-01",
  end_date = "2019-12"
)

location_set <- c("US", "CN", "JP")

# add synonyms -----------------------------------------------------------------
test_that("add_synonyms1", {
  out <- capture_messages(
    add_synonym(
      keyword = "fc bayern",
      synonym = c("bayern munich", "bayern munchen")
    )
  )

  expect_match(
    out,
    "Successfully added synonym | keyword: fc bayern | synonym: bayern munich\\.",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully added synonym | keyword: fc bayern | synonym: bayern munchen\\.",
    all = FALSE
  )

  expect_equal(nrow(gt.env$keyword_synonyms), 2)
})

# enter data -------------------------------------------------------------------
data <- filter(example_control, batch == 1 & location %in% location_set[1:3])
dbAppendTable(gt.env$globaltrends_db, "data_control", data)
data <- filter(
  example_object,
  batch_c == 1 & batch_o == 1 & location %in% location_set[1:2]
) %>%
  mutate(batch_o = 1)
dbAppendTable(gt.env$globaltrends_db, "data_object", data)
data <- filter(
  example_object,
  batch_c == 1 & batch_o == 2 & location %in% location_set[2:3]
) %>%
  mutate(batch_o = 2)
dbAppendTable(gt.env$globaltrends_db, "data_object", data)

compute_score(object = 1, locations = location_set[1:2])
out1 <- export_score(keyword = "fc bayern")

compute_score(object = 2, locations = location_set[1:3])
out2 <- export_score(keyword = "fc bayern")

# compare results --------------------------------------------------------------
test_that("keyword_score", {
  out1_cn <- out1 %>%
    filter(location == "CN") %>%
    summarise(score = mean(score), .groups = "drop")

  out2_cn <- out2 %>%
    filter(location == "CN") %>%
    summarise(score = mean(score), .groups = "drop")

  expect_gt(out2_cn$score, out1_cn$score)
})

test_that("keyword_synonym", {
  out2_cn <- gt.env$tbl_score %>%
    filter(keyword == "bayern munich" & location == "CN") %>%
    collect() %>%
    summarise(synonym = mean(synonym), .groups = "drop")

  expect_equal(out2_cn$synonym, 2)

  out2_jp <- gt.env$tbl_score %>%
    filter(keyword == "bayern munich" & location == "JP") %>%
    collect() %>%
    summarise(synonym = mean(synonym), .groups = "drop")

  expect_equal(out2_jp$synonym, 1)
})

# add synonyms signals ---------------------------------------------------------
test_that("add_synonyms2", {
  expect_error(
    add_synonym(keyword = letters[1:2], synonym = LETTERS[1:2]),
    "'keyword' must be object of length 1.\nYou provided an object of length 2."
  )
})

test_that("add_synonyms4", {
  test_keyword(fun = add_synonym, synonym = "A")
})

test_that("add_synonyms5", {
  expect_error(
    add_synonym(keyword = "A", synonym = 1),
    "no applicable method"
  )
  expect_error(
    add_synonym(keyword = "A", synonym = TRUE),
    "no applicable method"
  )
  expect_error(
    add_synonym(keyword = "A", synonym = sum),
    "no applicable method"
  )
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
Sys.unsetenv("LANGUAGE")
