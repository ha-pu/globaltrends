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
# scores needed by the synonym tests.

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

    ctrl_data <- example_control[example_control$batch == 1 & example_control$location %in% c("US", "CN", "JP"), ]
    gt.env$dt_control <- data.table::rbindlist(
      list(gt.env$dt_control, data.table::as.data.table(ctrl_data)),
      use.names = TRUE
    )

    obj1 <- example_object[example_object$batch_c == 1 & example_object$batch_o == 1 & example_object$location %in% c("US", "CN"), ]
    obj1$batch_o <- 1L
    gt.env$dt_object <- data.table::rbindlist(
      list(gt.env$dt_object, data.table::as.data.table(obj1)),
      use.names = TRUE
    )

    obj2 <- example_object[example_object$batch_c == 1 & example_object$batch_o == 2 & example_object$location %in% c("CN", "JP"), ]
    obj2$batch_o <- 2L
    gt.env$dt_object <- data.table::rbindlist(
      list(gt.env$dt_object, data.table::as.data.table(obj2)),
      use.names = TRUE
    )

    compute_score(object = 1:2, locations = c("US", "CN", "JP"))
  })
  invisible(NULL)
}
