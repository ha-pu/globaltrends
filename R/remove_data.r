#' @title Remove data from database tables
#'
#' @description
#' Removes batches and derived data from the database. Deletions are *greedy*:
#' all downstream tables that depend on the deleted entry are automatically
#' cleaned up to keep the database consistent.
#'
#' @details
#' ## Dependency chain
#'
#' Deletions cascade through the following dependency graph:
#'
#' ```
#' batch_keywords / batch_time
#'        |
#'        v
#'   data_control
#'        |
#'        v
#'   data_object ---> data_related
#'        |      \--> data_region
#'        v
#'   data_score
#'        |
#'        v
#'    data_doi
#' ```
#'
#' For example:
#' * Deleting a control batch from `data_control` removes all `data_object`
#'   rows for that control, then the associated `data_score`, `data_doi`,
#'   `data_related`, and `data_region` rows.
#' * Deleting an object batch from `batch_keywords` removes the corresponding
#'   `batch_time` entry, all `data_object` rows for that object batch, and
#'   everything downstream.
#'
#' ## Argument requirements by table
#'
#' | `table` | `control` | `object` |
#' |---|---|---|
#' | `"batch_keywords"`, `"batch_time"` | exactly one of | exactly one of |
#' | `"data_control"` | required | ignored |
#' | `"data_object"`, `"data_score"`, `"data_doi"` | at least one of | at least one of |
#' | `"data_related"`, `"data_region"` | ignored | required |
#'
#' After deletions, consider running [vacuum_data()] to reclaim disk space.
#' Vacuuming can take several minutes for large database files.
#'
#' @param table Character scalar. The table to delete from. One of
#'   `"batch_keywords"`, `"batch_time"`, `"data_control"`, `"data_object"`,
#'   `"data_score"`, `"data_doi"`, `"data_related"`, `"data_region"`.
#'   See the argument requirements table in Details for which of `control`
#'   and `object` are required, optional, or ignored for each table.
#'
#' @param control Optional integer-like scalar. Control batch id.
#'   * **Required** for `table = "data_control"`.
#'   * **Exactly one** of `control` or `object` for `"batch_keywords"` and
#'     `"batch_time"`.
#'   * **At least one** of `control` or `object` for `"data_object"`,
#'     `"data_score"`, and `"data_doi"`.
#'   * **Ignored** (with a warning) for `"data_related"` and `"data_region"`.
#'
#' @param object Optional integer-like scalar. Object batch id.
#'   * **Required** for `table = "data_related"` and `"data_region"`.
#'   * **Exactly one** of `control` or `object` for `"batch_keywords"` and
#'     `"batch_time"`.
#'   * **At least one** of `control` or `object` for `"data_object"`,
#'     `"data_score"`, and `"data_doi"`.
#'   * **Ignored** (with a warning) for `"data_control"`.
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
#' # Remove a control keyword batch and all data derived from it
#' remove_data(table = "batch_keywords", control = 1)
#'
#' # Remove an object keyword batch and all data derived from it
#' remove_data(table = "batch_keywords", object = 1)
#'
#' # Remove all object data linked to a control batch
#' remove_data(table = "data_object", control = 1)
#'
#' # Remove scores for one specific control-object combination
#' remove_data(table = "data_score", control = 1, object = 1)
#'
#' # Remove related-query data for an object batch
#' remove_data(table = "data_related", object = 1)
#'
#' # Remove regional breakdown data for an object batch
#' remove_data(table = "data_region", object = 1)
#'
#' # Reclaim disk space after bulk deletions
#' vacuum_data()
#' }
#'
#' @export
#' @rdname remove_data

remove_data <- function(table, control = NULL, object = NULL) {
  .check_length(table, 1)
  .check_input(table, "character")

  if (!is.null(control)) .check_length(control, 1)
  if (!is.null(object)) .check_length(object, 1)

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
      warning("`object` is ignored for `table = 'data_control'`.", call. = FALSE)
    }
    .remove_data_control(batch_c = control)
    return(invisible(TRUE))
  }

  # data_related and data_region require only object
  if (table %in% c("data_related", "data_region")) {
    if (is.null(object)) {
      stop(
        sprintf("For `table = '%s'`, `object` must be provided.", table),
        call. = FALSE
      )
    }
    if (!is.null(control)) {
      warning(
        sprintf("`control` is ignored for `table = '%s'`.", table),
        call. = FALSE
      )
    }
    if (table == "data_related") {
      .remove_data_related(batch_o = object)
    } else {
      .remove_data_region(batch_o = object)
    }
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
#' Reclaims unused disk space by running `VACUUM` on the underlying database.
#' Call this after bulk deletions via [remove_data()] to compact the file and
#' free storage.
#'
#' @details
#' For SQLite-based backends, `VACUUM` rewrites the entire database file in
#' place and may take several minutes for large databases. No data is modified;
#' only free pages are reclaimed.
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

#' @description Stops if both or neither of two optional arguments are non-NULL.
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

#' @description Stops if both of two optional arguments are NULL.
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

#' @description Validates `x` as a batch id only when `x` is non-NULL.
#' @keywords internal
#' @noRd

.check_batch_optional <- function(x) {
  if (!is.null(x)) .check_batch(x)
}

#' @description Validates both `batch_c` and `batch_o` when non-NULL.
#' @keywords internal
#' @noRd

.check_batches <- function(batch_c, batch_o) {
  .check_batch_optional(batch_c)
  .check_batch_optional(batch_o)
}

#' @description Executes a parameterized DELETE statement against the package
#'   database connection.
#' @keywords internal
#' @noRd
#' @importFrom DBI dbExecute

.db_delete <- function(statement, params = list()) {
  dbExecute(
    conn = gt.env$globaltrends_db,
    statement = statement,
    params = params
  )
}

#' @description Builds and executes a DELETE for tables with `batch_c`/`batch_o`
#'   columns, choosing the WHERE clause based on which identifiers are non-NULL.
#' @keywords internal
#' @noRd

.db_delete_by_batch <- function(table, batch_c, batch_o) {
  if (!is.null(batch_c) && is.null(batch_o)) {
    .db_delete(
      paste0("DELETE FROM ", table, " WHERE batch_c = ?"),
      params = list(batch_c)
    )
    message("Successfully deleted control batch ", batch_c, " from '", table, "'.")
  } else if (is.null(batch_c)) {
    .db_delete(
      paste0("DELETE FROM ", table, " WHERE batch_o = ?"),
      params = list(batch_o)
    )
    message("Successfully deleted object batch ", batch_o, " from '", table, "'.")
  } else {
    # Parameter order must match placeholder order
    .db_delete(
      paste0("DELETE FROM ", table, " WHERE batch_o = ? AND batch_c = ?"),
      params = list(batch_o, batch_c)
    )
    message(
      "Successfully deleted control batch ", batch_c,
      " and object batch ", batch_o, " from '", table, "'."
    )
  }
}

#' @description Deletes all rows from `table` matching `batch_o` and emits a
#'   confirmation message. Used by `.remove_data_related` and `.remove_data_region`.
#' @keywords internal
#' @noRd

.remove_data_by_batch_o <- function(table, batch_o) {
  .check_batch_optional(batch_o)
  .db_delete(
    paste0("DELETE FROM ", table, " WHERE batch_o = ?"),
    params = list(batch_o)
  )
  message("Successfully deleted object batch ", batch_o, " from '", table, "'.")
}

#' @description Deletes one batch entry from `batch_keywords`, refreshes the
#'   in-memory keyword list in `gt.env`, and cascades greedy deletions to
#'   `data_control` or `data_object` (depending on `type`) and `batch_time`.
#' @keywords internal
#' @noRd

.remove_batch_keywords <- function(type, batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)

  batch <- if (type == "control") batch_c else batch_o
  .db_delete(
    "DELETE FROM batch_keywords WHERE type = ? AND batch = ?",
    params = list(type, batch)
  )

  # Refresh keyword lists in gt.env so downstream calls see a consistent state
  .refresh_keywords(type)

  message("Successfully deleted ", type, " batch ", batch, " from 'batch_keywords'.")

  # Greedy deletion: remove dependent data and time windows
  if (type == "control") {
    .remove_data_control(batch_c = batch_c)
  } else {
    .remove_data_object(batch_o = batch_o)
  }

  .remove_batch_time(type = type, batch_c = batch_c, batch_o = batch_o)
}

#' @description Deletes one batch entry from `batch_time` and refreshes the
#'   in-memory time list in `gt.env`. Not greedy: no downstream tables are touched.
#' @keywords internal
#' @noRd

.remove_batch_time <- function(type, batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)

  batch <- if (type == "control") batch_c else batch_o
  .db_delete(
    "DELETE FROM batch_time WHERE type = ? AND batch = ?",
    params = list(type, batch)
  )

  .refresh_time(type)

  message("Successfully deleted ", type, " batch ", batch, " from 'batch_time'.")
}

#' @description Deletes from `data_control` for `batch_c` and cascades greedy
#'   deletions to `data_object` (and everything downstream).
#' @keywords internal
#' @noRd

.remove_data_control <- function(batch_c) {
  .check_batch_optional(batch_c)
  .db_delete(
    "DELETE FROM data_control WHERE batch = ?",
    params = list(batch_c)
  )
  message("Successfully deleted control batch ", batch_c, " from 'data_control'.")

  # Greedy: object rows reference control batches
  .remove_data_object(batch_c = batch_c)
}

#' @description Deletes from `data_object` for the given batch identifiers and
#'   cascades greedy deletions to `data_score`, `data_related`, and `data_region`.
#' @keywords internal
#' @noRd

.remove_data_object <- function(batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")
  .db_delete_by_batch("data_object", batch_c, batch_o)
  .remove_data_score(batch_c = batch_c, batch_o = batch_o)
  .remove_data_related(batch_o = batch_o)
  .remove_data_region(batch_o = batch_o)
}

#' @description Deletes from `data_score` for the given batch identifiers and
#'   cascades greedy deletions to `data_doi`.
#' @keywords internal
#' @noRd

.remove_data_score <- function(batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")
  .db_delete_by_batch("data_score", batch_c, batch_o)
  .remove_data_doi(batch_c = batch_c, batch_o = batch_o)
}

#' @description Deletes from `data_doi` for the given batch identifiers.
#'   Terminal node of the greedy cascade: no further tables are touched.
#' @keywords internal
#' @noRd

.remove_data_doi <- function(batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")
  .db_delete_by_batch("data_doi", batch_c, batch_o)
}

#' @description Deletes all `data_related` rows for the given object batch.
#' @keywords internal
#' @noRd

.remove_data_related <- function(batch_o = NULL) .remove_data_by_batch_o("data_related", batch_o)

#' @description Deletes all `data_region` rows for the given object batch.
#' @keywords internal
#' @noRd

.remove_data_region <- function(batch_o = NULL) .remove_data_by_batch_o("data_region", batch_o)

#' @description Re-queries `batch_keywords` for the given type and updates
#'   `gt.env$keywords_<type>` so downstream calls see a consistent state.
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter select
#' @importFrom rlang .data

.refresh_keywords <- function(x.type) {
  df <- gt.env$tbl_keywords |>
    filter(.data$type == x.type) |>
    select(-.data$type) |>
    collect()

  assign(paste0("keywords_", x.type), df, envir = gt.env)
  invisible(TRUE)
}

#' @description Re-queries `batch_time` for the given type and updates
#'   `gt.env$time_<type>` so downstream calls see a consistent state.
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter select
#' @importFrom rlang .data

.refresh_time <- function(x.type) {
  df <- gt.env$tbl_time |>
    filter(.data$type == x.type) |>
    select(-.data$type) |>
    collect()

  assign(paste0("time_", x.type), df, envir = gt.env)
  invisible(TRUE)
}
