# setup ------------------------------------------------------------------------
suppressWarnings(library(DBI))

initialize_db()
start_db()

# enter data -------------------------------------------------------------------
dbWriteTable(globaltrends_db, "data_score", data_score, append = TRUE)
dbWriteTable(globaltrends_db, "data_doi", data_doi, append = TRUE)

# export voi change ------------------------------------------------------------
test_that("voi1", {
  out <- export_voi_change(keyword = "amazon", control = 1)
  expect_length(na.omit(out$voi_change), 119)
  expect_length(na.omit(out$quantile), 119)
})

test_that("voi2", {
  out <- export_voi_change(keyword = "amazon", control = 1, type = "obs")
  expect_length(na.omit(out$voi_change), 119)
  expect_length(na.omit(out$quantile), 119)
})

test_that("voi3", {
  out <- export_voi_change(keyword = "amazon", control = 1, type = "sad")
  expect_length(na.omit(out$voi_change), 119)
  expect_length(na.omit(out$quantile), 119)
})

test_that("voi4", {
  out <- export_voi_change(keyword = "amazon", control = 1, type = "trd")
  expect_length(na.omit(out$voi_change), 119)
  expect_length(na.omit(out$quantile), 119)
})

test_that("voi5", {
  out1 <- export_voi_change(keyword = "amazon", control = 1)
  out2 <- export_voi_change(keyword = "amazon", control = 1, type = "obs")
  out3 <- export_voi_change(keyword = "amazon", control = 1, type = "sad")
  out4 <- export_voi_change(keyword = "amazon", control = 1, type = "trd")

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("voi6", {
  out <- export_voi_change(object = 1, control = 1)
  expect_length(na.omit(out$voi_change), 476)
  expect_length(na.omit(out$quantile), 476)
})

# export doi change ------------------------------------------------------------
test_that("doi1", {
  out <- export_doi_change(keyword = "amazon", control = 1)
  expect_length(na.omit(out$doi_change), 357)
  expect_length(na.omit(out$quantile), 357)
})

test_that("doi2", {
  out <- export_doi_change(keyword = "amazon", control = 1, measure = "gini")
  expect_length(na.omit(out$doi_change), 357)
  expect_length(na.omit(out$quantile), 357)
})

test_that("doi3", {
  out <- export_doi_change(keyword = "amazon", control = 1, measure = "hhi")
  expect_length(na.omit(out$doi_change), 357)
  expect_length(na.omit(out$quantile), 357)
})

test_that("doi4", {
  out <- export_doi_change(keyword = "amazon", control = 1, measure = "entropy")
  expect_length(na.omit(out$doi_change), 357)
  expect_length(na.omit(out$quantile), 357)
})

test_that("doi5", {
  out1 <- export_doi_change(keyword = "amazon", control = 1)
  out2 <- export_doi_change(keyword = "amazon", control = 1, measure = "gini")
  out3 <- export_doi_change(keyword = "amazon", control = 1, measure = "hhi")
  out4 <- export_doi_change(keyword = "amazon", control = 1, measure = "entropy")

  expect_identical(out1, out2)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

test_that("doi6", {
  out <- export_doi_change(object = 1, control = 1)
  expect_length(na.omit(out$doi_change), 1428)
  expect_length(na.omit(out$quantile), 1428)
})

# plot voi change --------------------------------------------------------------
test_that("plot_voi1", {
  data <- export_voi_change(keyword = "fc barcelona", type = "obs")
  out1 <- plot_voi_change(data_change = data)
  expect_s3_class(out1, "ggplot")

  data <- export_voi_change(object = 1, type = "obs")
  expect_warning(
    out2 <- plot_voi_change(data_change = data),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi2", {
  data <- export_voi_change(keyword = "fc barcelona", type = "sad")
  out1 <- plot_voi_change(data_change = data)
  expect_s3_class(out1, "ggplot")

  data <- export_voi_change(object = 1, type = "sad")
  expect_warning(
    out2 <- plot_voi_change(data_change = data),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_voi3", {
  data <- export_voi_change(keyword = "fc barcelona", type = "trd")
  out1 <- plot_voi_change(data_change = data)
  expect_s3_class(out1, "ggplot")

  data <- export_voi_change(object = 1, type = "trd")
  expect_warning(
    out2 <- plot_voi_change(data_change = data),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot doi change gini ---------------------------------------------------------
test_that("plot_doi1a", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "gini")
  out1 <- plot_doi_change(data_change = data, type = "obs")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "gini")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "obs"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_doi1b", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "gini")
  out1 <- plot_doi_change(data_change = data, type = "sad")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "gini")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "sad"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_doi1c", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "gini")
  out1 <- plot_doi_change(data_change = data, type = "trd")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "gini")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "trd"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot doi change hhi ----------------------------------------------------------
test_that("plot_doi2a", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "hhi")
  out1 <- plot_doi_change(data_change = data, type = "obs")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "hhi")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "obs"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_doi2b", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "hhi")
  out1 <- plot_doi_change(data_change = data, type = "sad")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "hhi")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "sad"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_doi2c", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "hhi")
  out1 <- plot_doi_change(data_change = data, type = "trd")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "hhi")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "trd"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot doi change entropy ------------------------------------------------------
test_that("plot_doi3a", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "entropy")
  out1 <- plot_doi_change(data_change = data, type = "obs")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "entropy")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "obs"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_doi3b", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "entropy")
  out1 <- plot_doi_change(data_change = data, type = "sad")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "entropy")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "sad"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

test_that("plot_doi3c", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "entropy")
  out1 <- plot_doi_change(data_change = data, type = "trd")
  expect_s3_class(out1, "ggplot")

  data <- export_doi_change(object = 1, measure = "entropy")
  expect_warning(
    out2 <- plot_doi_change(data_change = data, type = "trd"),
    "The plot function is limited to 1 keyword\\.\nYou use 4 keywords\\.\nOnly the first keyword is used\\."
  )
  expect_s3_class(out2, "ggplot")

  expect_identical(out1$labels, out2$labels)
})

# plot doi change defaults -----------------------------------------------------
test_that("plot_doi_def", {
  data <- export_doi_change(keyword = "fc barcelona", measure = "gini")
  out1 <- plot_doi_change(data_change = data)
  out2 <- plot_doi_change(data_change = data, type = "obs")
  out3 <- plot_doi_change(data_change = data, type = "sad")
  out4 <- plot_doi_change(data_change = data, type = "trd")

  expect_identical(out1$labels, out2$labels)
  expect_false(identical(out2, out3))
  expect_false(identical(out2, out4))
  expect_false(identical(out3, out4))
})

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
