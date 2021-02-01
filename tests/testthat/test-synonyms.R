# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))
suppressWarnings(library(dplyr))

initialize_db()
start_db()

add_control_keyword(
  keyword = c("gmail", "map", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = list(
    c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
    c("bayern munich", "bayern munchen")
  ),
  time = "2010-01-01 2019-12-31"
)

# add synonyms -----------------------------------------------------------------
test_that("add_synonyms", {
  out <- capture_messages(
    add_synonym(
      keyword = "fc bayern",
      synonym = c("bayern munich", "bayern munchen")
    )
  )

  expect_match(
    out,
    "Successfully added synonym | keyword: fc bayern | synonym: bayern munich",
    all = FALSE
  )

  expect_match(
    out,
    "Successfully added synonym | keyword: fc bayern | synonym: bayern munchen",
    all = FALSE
  )

  expect_equal(nrow(keyword_synonyms), 2)
})

# enter data -------------------------------------------------------------------
data <- filter(example_control, batch == 1 & location %in% countries[1:3])
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(
  example_object,
  batch_c == 1 & batch_o == 1 &
    location %in% countries[1:2]
) %>%
  mutate(batch_o = 1)
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)
data <- filter(
  example_object,
  batch_c == 1 & batch_o == 2 &
    location %in% countries[2:3]
) %>%
  mutate(batch_o = 2)
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)

compute_score(object = 1, locations = countries[1:2])
out1 <- export_score(keyword = "fc bayern")

compute_score(object = 2, locations = countries[1:3])
out2 <- export_score(keyword = "fc bayern")

# compare results --------------------------------------------------------------
test_that("keyword_score", {
  out1_cn <- out1 %>%
    filter(location == "CN") %>%
    summarise(score = mean(score_obs), .groups = "drop")

  out2_cn <- out2 %>%
    filter(location == "CN") %>%
    summarise(score = mean(score_obs), .groups = "drop")

  expect_gt(out2_cn$score, out1_cn$score)
})

test_that("keyword_synonym", {
  out2_cn <- .tbl_score %>%
    filter(keyword == "bayern munich" & location == "CN") %>%
    collect() %>%
    summarise(synonym = mean(synonym), .groups = "drop")

  expect_equal(out2_cn$synonym, 0)

  out2_jp <- .tbl_score %>%
    filter(keyword == "bayern munich" & location == "JP") %>%
    collect() %>%
    summarise(synonym = mean(synonym), .groups = "drop")

  expect_equal(out2_jp$synonym, 1)
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
