# Auto-loaded by testthat before every test file.
# Provides local_db() for fully-isolated database setup in a temp directory.
# Cleanup order (LIFO): disconnect_db → restore CWD → delete temp dir.

local_db <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_dir(dir, .local_envir = env)
  suppressMessages(initialize_db())
  suppressMessages(start_db())
  withr::defer(suppressMessages(disconnect_db()), envir = env)
  invisible(dir)
}

# Extends local_db() with the keyword batches, raw download data, and computed
# scores needed by the synonym tests. Two object batches are created so that
# synonym aggregation has something to roll up:
#   batch 1 — fc barcelona / fc bayern / manchester united / real madrid (US + CN)
#   batch 2 — bayern munich / bayern munchen (CN + JP)
# After compute_score(), batch 1 has US+CN scores and batch 2 has CN+JP scores.

local_synonyms_db <- function(env = parent.frame()) {
  local_db(env = env)
  suppressMessages({
    add_control_keyword(
      keyword    = c("gmail", "map", "wikipedia", "youtube"),
      start_date = "2010-01",
      end_date   = "2019-12"
    )
    add_object_keyword(
      keyword = list(
        c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
        c("bayern munich", "bayern munchen")
      ),
      start_date = "2010-01",
      end_date = "2019-12"
    )
    DBI::dbAppendTable(
      gt.env$globaltrends_db, "data_control",
      dplyr::filter(example_control, batch == 1 & location %in% c("US", "CN", "JP"))
    )
    DBI::dbAppendTable(
      gt.env$globaltrends_db, "data_object",
      dplyr::mutate(
        dplyr::filter(example_object, batch_c == 1 & batch_o == 1 & location %in% c("US", "CN")),
        batch_o = 1
      )
    )
    DBI::dbAppendTable(
      gt.env$globaltrends_db, "data_object",
      dplyr::mutate(
        dplyr::filter(example_object, batch_c == 1 & batch_o == 2 & location %in% c("CN", "JP")),
        batch_o = 2
      )
    )
    compute_score(object = 1:2, locations = c("US", "CN", "JP"))
  })
  invisible(NULL)
}
