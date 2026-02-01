#' @title Remove data from database tables
#'
#' @description
#' Removes batches and derived data from the database. Deletions are *greedy*:
#' downstream tables that depend on the deleted batches are cleaned up
#' automatically to keep the database consistent.
#'
#' @details
#' The dependency chain is:
#' `batch_keywords` / `batch_time` \eqn{\rightarrow} `data_control`
#' \eqn{\rightarrow} `data_object` \eqn{\rightarrow} `data_score`
#' \eqn{\rightarrow} `data_doi`.
#'
#' Examples:
#' * Removing a control batch from `data_control` also removes all object rows
#'   referencing that control batch, then associated scores and DOI rows.
#' * Removing an object batch from `batch_keywords` removes the corresponding
#'   object data (`data_object`), then scores and DOI rows derived from it.
#'
#' After deletions, consider running [vacuum_data()] to reclaim disk space.
#' Vacuuming can take several minutes for large database files.
#'
#' @param table Character scalar. One of:
#'   `"batch_keywords"`, `"batch_time"`, `"data_control"`, `"data_object"`,
#'   `"data_score"`, `"data_doi"`.
#'
#' @param control Optional numeric/integer scalar. Control batch id. Required
#'   for `table = "data_control"`. For `batch_keywords` and `batch_time`, exactly
#'   one of `control` or `object` must be provided.
#'
#' @param object Optional numeric/integer scalar. Object batch id. For
#'   `batch_keywords` and `batch_time`, exactly one of `control` or `object`
#'   must be provided.
#'
#' @return
#' Invisibly returns `TRUE` on success. The function is called for its side
#' effects (deleting rows).
#'
#' @seealso
#' * [vacuum_data()]
#' * [example_keywords()]
#' * [example_time()]
#' * [example_control()]
#' * [example_object()]
#' * [example_score()]
#' * [example_doi()]
#'
#' @examples
#' \dontrun{
#' remove_data(table = "batch_keywords", control = 1)
#' remove_data(table = "data_score", control = 1, object = 1)
#' vacuum_data()
#' }
#'
#' @export
#' @importFrom DBI dbExecute
#' @importFrom dplyr collect filter select
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @rdname remove_data

remove_data <- function(table, control = NULL, object = NULL) {
  .check_length(table, 1)
  .check_input(table, "character")

  # Validate scalar-ness only if provided
  if (!is.null(control)) {
    .check_length(control, 1)
  }
  if (!is.null(object)) {
    .check_length(object, 1)
  }

  allowed <- c(
    "batch_keywords",
    "batch_time",
    "data_control",
    "data_object",
    "data_score",
    "data_doi",
    "data_related",
    "data_region"
  )
  if (!(table %in% allowed)) {
    stop(
      paste0(
        "`table` must be one of: ",
        paste(shQuote(allowed), collapse = ", "),
        ". You provided ",
        shQuote(table),
        "."
      ),
      call. = FALSE
    )
  }

  # Dispatch with table-specific argument rules
  if (table %in% c("batch_keywords", "batch_time")) {
    .require_exactly_one(control, object, arg1 = "control", arg2 = "object")
    type <- if (!is.null(control)) "control" else "object"

    if (table == "batch_keywords") {
      .remove_batch_keywords(type = type, batch_c = control, batch_o = object)
    } else {
      .remove_batch_time(type = type, batch_c = control, batch_o = object)
    }

    return(invisible(TRUE))
  }

  if (table == "data_control") {
    if (is.null(control)) {
      stop(
        "For `table = 'data_control'`, `control` must be provided.",
        call. = FALSE
      )
    }
    if (!is.null(object)) {
      warning(
        "`object` is ignored for `table = 'data_control'`.",
        call. = FALSE
      )
    }
    .remove_data_control(batch_c = control)
    return(invisible(TRUE))
  }

  # data_related and data_region require only object
  if (table == "data_related") {
    if (is.null(object)) {
      stop(
        "For `table = 'data_related'`, `object` must be provided.",
        call. = FALSE
      )
    }
    if (!is.null(object)) {
      warning(
        "`control` is ignored for `table = 'data_related'`.",
        call. = FALSE
      )
    }
    .remove_data_related(batch_o = object)
    return(invisible(TRUE))
  }
  if (table == "data_region") {
    if (is.null(object)) {
      stop(
        "For `table = 'data_region'`, `object` must be provided.",
        call. = FALSE
      )
    }
    if (!is.null(control)) {
      warning(
        "`control` is ignored for `table = 'data_region'`.",
        call. = FALSE
      )
    }
    .remove_data_region(batch_o = object)
    return(invisible(TRUE))
  }

  # Remaining tables accept control, object, or both, but not neither
  .require_at_least_one(control, object, arg1 = "control", arg2 = "object")

  if (table == "data_object") {
    .remove_data_object(batch_c = control, batch_o = object)
  } else if (table == "data_score") {
    .remove_data_score(batch_c = control, batch_o = object)
  } else if (table == "data_doi") {
    .remove_data_doi(batch_c = control, batch_o = object)
  }

  invisible(TRUE)
}

#' @title Vacuum database file
#'
#' @description
#' Executes `VACUUM` on the underlying database to reclaim unused space after
#' large deletions.
#'
#' @details
#' For SQLite-based backends, `VACUUM` rewrites the database file and can take
#' noticeable time for large databases.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#' @rdname remove_data
#' @importFrom DBI dbExecute

vacuum_data <- function() {
  dbExecute(conn = gt.env$globaltrends_db, statement = "VACUUM")
  message("Vacuum completed successfully.")
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd

.require_exactly_one <- function(x, y, arg1, arg2) {
  if (is.null(x) && is.null(y)) {
    stop(
      paste0("Specify exactly one of `", arg1, "` or `", arg2, "`."),
      call. = FALSE
    )
  }
  if (!is.null(x) && !is.null(y)) {
    stop(
      paste0("Specify only one of `", arg1, "` or `", arg2, "`, not both."),
      call. = FALSE
    )
  }
}

#' @keywords internal
#' @noRd

.require_at_least_one <- function(x, y, arg1, arg2) {
  if (is.null(x) && is.null(y)) {
    stop(
      paste0("Specify at least one of `", arg1, "` or `", arg2, "`."),
      call. = FALSE
    )
  }
}

#' @keywords internal
#' @noRd

.check_batch_optional <- function(x) {
  if (!is.null(x)) .check_batch(x)
}

#' @keywords internal
#' @noRd
.db_delete <- function(statement, params = list()) {
  dbExecute(
    conn = gt.env$globaltrends_db,
    statement = statement,
    params = params
  )
}

#' @title Remove from batch_keywords (greedy)
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter select
#' @importFrom purrr walk
#' @importFrom rlang .data

.remove_batch_keywords <- function(type, batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch_optional)

  batch <- if (type == "control") batch_c else batch_o
  .db_delete(
    statement = "DELETE FROM batch_keywords WHERE type = ? AND batch = ?",
    params = list(type, batch)
  )

  # Refresh keyword lists in gt.env so downstream calls see a consistent state
  .refresh_keywords(type)

  message(
    paste0(
      "Successfully deleted ",
      type,
      " batch ",
      batch,
      " from 'batch_keywords'."
    )
  )

  # Greedy deletion: remove dependent data and time windows
  if (type == "control") {
    .remove_data_control(batch_c = batch_c)
  } else {
    .remove_data_object(batch_o = batch_o)
  }

  .remove_batch_time(type = type, batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from batch_time
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter select
#' @importFrom purrr walk
#' @importFrom rlang .data

.remove_batch_time <- function(type, batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch_optional)

  batch <- if (type == "control") batch_c else batch_o
  .db_delete(
    statement = "DELETE FROM batch_time WHERE type = ? AND batch = ?",
    params = list(type, batch)
  )

  .refresh_time(type)

  message(
    paste0(
      "Successfully deleted ",
      type,
      " batch ",
      batch,
      " from 'batch_time'."
    )
  )
}

#' @title Remove from data_control (greedy)
#' @keywords internal
#' @noRd

.remove_data_control <- function(batch_c) {
  .check_batch_optional(batch_c)

  .db_delete(
    statement = "DELETE FROM data_control WHERE batch = ?",
    params = list(batch_c)
  )
  message(paste0(
    "Successfully deleted control batch ",
    batch_c,
    " from 'data_control'."
  ))

  # Greedy: object rows reference control batches
  .remove_data_object(batch_c = batch_c)
}

#' @title Remove from data_object (greedy)
#' @keywords internal
#' @noRd
#' @importFrom purrr walk

.remove_data_object <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch_optional)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")

  if (!is.null(batch_c) && is.null(batch_o)) {
    .db_delete(
      "DELETE FROM data_object WHERE batch_c = ?",
      params = list(batch_c)
    )
    message(paste0(
      "Successfully deleted control batch ",
      batch_c,
      " from 'data_object'."
    ))
  } else if (is.null(batch_c) && !is.null(batch_o)) {
    .db_delete(
      "DELETE FROM data_object WHERE batch_o = ?",
      params = list(batch_o)
    )
    message(paste0(
      "Successfully deleted object batch ",
      batch_o,
      " from 'data_object'."
    ))
  } else {
    # Important: parameter order must match placeholder order
    .db_delete(
      "DELETE FROM data_object WHERE batch_o = ? AND batch_c = ?",
      params = list(batch_o, batch_c)
    )
    message(
      paste0(
        "Successfully deleted control batch ",
        batch_c,
        " and object batch ",
        batch_o,
        " from 'data_object'."
      )
    )
  }

  .remove_data_score(batch_c = batch_c, batch_o = batch_o)
  .remove_data_related(batch_o = batch_o)
  .remove_data_region(batch_o = batch_o)
}

#' @title Remove from data_score (greedy)
#' @keywords internal
#' @noRd
#' @importFrom purrr walk

.remove_data_score <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch_optional)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")

  if (!is.null(batch_c) && is.null(batch_o)) {
    .db_delete(
      "DELETE FROM data_score WHERE batch_c = ?",
      params = list(batch_c)
    )
    message(paste0(
      "Successfully deleted control batch ",
      batch_c,
      " from 'data_score'."
    ))
  } else if (is.null(batch_c) && !is.null(batch_o)) {
    .db_delete(
      "DELETE FROM data_score WHERE batch_o = ?",
      params = list(batch_o)
    )
    message(paste0(
      "Successfully deleted object batch ",
      batch_o,
      " from 'data_score'."
    ))
  } else {
    .db_delete(
      "DELETE FROM data_score WHERE batch_o = ? AND batch_c = ?",
      params = list(batch_o, batch_c)
    )
    message(
      paste0(
        "Successfully deleted control batch ",
        batch_c,
        " and object batch ",
        batch_o,
        " from 'data_score'."
      )
    )
  }

  .remove_data_doi(batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from data_doi
#' @keywords internal
#' @noRd
#' @importFrom purrr walk

.remove_data_doi <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch_optional)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")

  if (!is.null(batch_c) && is.null(batch_o)) {
    .db_delete("DELETE FROM data_doi WHERE batch_c = ?", params = list(batch_c))
    message(paste0(
      "Successfully deleted control batch ",
      batch_c,
      " from 'data_doi'."
    ))
  } else if (is.null(batch_c) && !is.null(batch_o)) {
    .db_delete("DELETE FROM data_doi WHERE batch_o = ?", params = list(batch_o))
    message(paste0(
      "Successfully deleted object batch ",
      batch_o,
      " from 'data_doi'."
    ))
  } else {
    .db_delete(
      "DELETE FROM data_doi WHERE batch_o = ? AND batch_c = ?",
      params = list(batch_o, batch_c)
    )
    message(
      paste0(
        "Successfully deleted control batch ",
        batch_c,
        " and object batch ",
        batch_o,
        " from 'data_doi'."
      )
    )
  }
}

#' @title Remove from data_related
#' @keywords internal
#' @noRd
#' @importFrom purrr walk

.remove_data_related <- function(batch_o = NULL) {
  .check_batch_optional(batch_o)

  .db_delete(
    statement = "DELETE FROM data_related WHERE batch_o = ?",
    params = list(batch_o)
  )
  message(paste0(
    "Successfully deleted object batch ",
    batch_o,
    " from 'data_related'."
  ))
}

#' @title Remove from data_region
#' @keywords internal
#' @noRd
#' @importFrom purrr walk

.remove_data_region <- function(batch_o = NULL) {
  .check_batch_optional(batch_o)

  .db_delete(
    statement = "DELETE FROM data_region WHERE batch_o = ?",
    params = list(batch_o)
  )
  message(paste0(
    "Successfully deleted object batch ",
    batch_o,
    " from 'data_region'."
  ))
}

#' @title Refresh keyword vectors in gt.env
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter select
#' @importFrom rlang .data

.refresh_keywords <- function(type) {
  df <- gt.env$tbl_keywords |>
    filter(.data$type == type) |>
    select(-.data$type) |>
    collect()

  assign(paste0("keywords_", type), df, envir = gt.env)
  invisible(TRUE)
}

#' @title Refresh time vectors in gt.env
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter select
#' @importFrom rlang .data

.refresh_time <- function(type) {
  df <- gt.env$tbl_time |>
    filter(.data$type == type) |>
    select(-.data$type) |>
    collect()

  assign(paste0("time_", type), df, envir = gt.env)
  invisible(TRUE)
}
