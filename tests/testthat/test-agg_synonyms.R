# setup ----

initialize_db()
start_db()

add_control_keyword(
  keyword = c("gmail", "map", "wikipedia", "youtube"),
  time = "2010-01-01 2019-12-31"
)

add_object_keyword(
  keyword = list(
    c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
    c("bayern munich", "bayern münchen")
  ),
  time = "2010-01-01 2019-12-31"
)

# add synonyms ----
test_that("add_synonyms", {
  expect_message(
    add_synonym(
      keyword = "fc bayern",
      synonym = c("bayern munich", "bayern münchen")
    )
  )

  expect_length(nrow(keyword_synonyms), 2)
})

# download data ----
download_control(control = 1, locations = countries[1:3])
download_object(control = 1, object = 1, locations = countries[1:2])
download_object(object = 2, locations = countries[2:3])

compute_score(control = 1, object = 1, locations = countries[1:2])
out1 <- export_score(control = 1, object = 1)

compute_score(control = 1, object = 2, locations = countries[1:2])
out2 <- export_score(control = 1)

# compare results ----
test_that("keyword_score", {
  out1_cn <- out1 %>%
    filter(keyword == "fc bayern" & location == "CN") %>%
    summarise(score = mean(score_obs), .groups = "drop")

  out2_cn <- out2 %>%
    filter(keyword == "fc bayern" & location == "CN") %>%
    summarise(score = mean(score_obs), .groups = "drop")

  expect_gt(out1_cn$score, out2_cn$score)
})

test_that("keyword_synonym", {
  out2_cn <- out2 %>%
    filter(keyword == "bayern munich" & location == "CN") %>%
    summarise(synonym = mean(synonym), .groups = "drop")

  expect_equal(out2_cn$synonym, 1)

  out2_jp <- out2 %>%
    filter(keyword == "bayern munich" & location == "JP") %>%
    summarise(synonym = mean(synonym), .groups = "drop")

  expect_equal(out2_jp$synonym, 0)
})

# disconnect ----
disconnect_db()
