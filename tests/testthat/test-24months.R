# setup ------------------------------------------------------------------------
library(DBI)
library(dplyr)
library(lubridate)

initialize_db()
start_db()

# enter data -------------------------------------------------------------------
data <- filter(data_control, batch == 1 & year(as_date(date)) == 2019)
dbWriteTable(globaltrends_db, "data_control", data, append = TRUE)
data <- filter(data_object, batch_c == 1 & batch_o == 1  & year(as_date(date)) == 2019)
dbWriteTable(globaltrends_db, "data_object", data, append = TRUE)

# compute_score ----------------------------------------------------------------
test_that("score1", {
  expect_message(compute_score(control = 1, object = 1))
})

test_that("score2", {
  expect_message(compute_voi(control = 1, object = 1))
})

# compute_doi ------------------------------------------------------------------

# plot_score -------------------------------------------------------------------

# plot_doi ---------------------------------------------------------------------

# plot_voi ---------------------------------------------------------------------

# plot_voi_doi -----------------------------------------------------------------

# disconnect -------------------------------------------------------------------
disconnect_db()
unlink("db", recursive = TRUE)
