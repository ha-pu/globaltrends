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
#' @param table Character scalar. The table to delete from. One of
#'   `"batch_keywords"`, `"batch_time"`, `"data_control"`, `"data_object"`,
#'   `"data_score"`, `"data_doi"`, `"data_related"`, `"data_region"`.
#'
#' @param control Optional integer-like scalar. Control batch id.
#'
#' @param object Optional integer-like scalar. Object batch id.
#'
#' @return
#' Invisibly returns `TRUE` on success. The function is called for its side
#' effects (deleting rows).
#'
#' @examples
#' \dontrun{
#' remove_data(table = "batch_keywords", control = 1)
#' remove_data(table = "batch_keywords", object = 1)
#' remove_data(table = "data_object", control = 1)
#' remove_data(table = "data_score", control = 1, object = 1)
#' remove_data(table = "data_related", object = 1)
#' remove_data(table = "data_region", object = 1)
#' }
#'
#' @export
#' @rdname remove_data

remove_data <- function(table, control = NULL, object = NULL) {
  .check_length(table, 1)
  .check_input(table, "character")

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


# -------------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------------

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

#' @noRd
.require_at_least_one <- function(x, y, arg1, arg2) {
  if (is.null(x) && is.null(y)) {
    stop(
      paste0("Specify at least one of `", arg1, "` or `", arg2, "`."),
      call. = FALSE
    )
  }
}

#' @noRd
.check_batch_optional <- function(x) {
  if (!is.null(x)) .check_batch(x)
}

#' @noRd
.check_batches <- function(batch_c, batch_o) {
  .check_batch_optional(batch_c)
  .check_batch_optional(batch_o)
}

#' @noRd
.dt_delete_by_batch <- function(table, batch_c, batch_o) {
  slot <- .table_slot(table)
  dt <- gt.env[[slot]]

  if (!is.null(batch_c) && is.null(batch_o)) {
    col <- if ("batch_c" %in% names(dt)) "batch_c" else "batch"
    gt.env[[slot]] <- dt[dt[[col]] != batch_c, ]
    message(
      "Successfully deleted control batch ",
      batch_c,
      " from '",
      table,
      "'."
    )
  } else if (is.null(batch_c)) {
    gt.env[[slot]] <- dt[dt$batch_o != batch_o, ]
    message(
      "Successfully deleted object batch ",
      batch_o,
      " from '",
      table,
      "'."
    )
  } else {
    col <- if ("batch_c" %in% names(dt)) "batch_c" else "batch"
    gt.env[[slot]] <- dt[!(dt[[col]] == batch_c & dt$batch_o == batch_o), ]
    message(
      "Successfully deleted control batch ",
      batch_c,
      " and object batch ",
      batch_o,
      " from '",
      table,
      "'."
    )
  }
}

#' @noRd
.remove_data_by_batch_o <- function(table, batch_o) {
  if (is.null(batch_o)) {
    return(invisible(NULL))
  }
  .check_batch_optional(batch_o)
  slot <- .table_slot(table)
  dt <- gt.env[[slot]]
  gt.env[[slot]] <- dt[dt$batch_o != batch_o, ]
  message("Successfully deleted object batch ", batch_o, " from '", table, "'.")
}

#' @noRd
.remove_batch_keywords <- function(type, batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)

  batch <- if (type == "control") batch_c else batch_o
  dt <- gt.env$dt_keywords
  gt.env$dt_keywords <- dt[!(dt$type == type & dt$batch == batch), ]

  .refresh_keywords(type)

  message(
    "Successfully deleted ",
    type,
    " batch ",
    batch,
    " from 'batch_keywords'."
  )

  if (type == "control") {
    .remove_data_control(batch_c = batch_c)
  } else {
    .remove_data_object(batch_o = batch_o)
  }

  .remove_batch_time(type = type, batch_c = batch_c, batch_o = batch_o)
}

#' @noRd
.remove_batch_time <- function(type, batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)

  batch <- if (type == "control") batch_c else batch_o
  dt <- gt.env$dt_time
  gt.env$dt_time <- dt[!(dt$type == type & dt$batch == batch), ]

  .refresh_time(type)

  message(
    "Successfully deleted ",
    type,
    " batch ",
    batch,
    " from 'batch_time'."
  )
}

#' @noRd
.remove_data_control <- function(batch_c) {
  .check_batch_optional(batch_c)
  dt <- gt.env$dt_control
  gt.env$dt_control <- dt[dt$batch != batch_c, ]
  message(
    "Successfully deleted control batch ",
    batch_c,
    " from 'data_control'."
  )

  .remove_data_object(batch_c = batch_c)
}

#' @noRd
.remove_data_object <- function(batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")
  .dt_delete_by_batch("data_object", batch_c, batch_o)
  .remove_data_score(batch_c = batch_c, batch_o = batch_o)
  .remove_data_related(batch_o = batch_o)
  .remove_data_region(batch_o = batch_o)
}

#' @noRd
.remove_data_score <- function(batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")
  .dt_delete_by_batch("data_score", batch_c, batch_o)
  .remove_data_doi(batch_c = batch_c, batch_o = batch_o)
}

#' @noRd
.remove_data_doi <- function(batch_c = NULL, batch_o = NULL) {
  .check_batches(batch_c, batch_o)
  .require_at_least_one(batch_c, batch_o, arg1 = "batch_c", arg2 = "batch_o")
  .dt_delete_by_batch("data_doi", batch_c, batch_o)
}

#' @noRd
.remove_data_related <- function(batch_o = NULL) {
  .remove_data_by_batch_o("data_related", batch_o)
}

#' @noRd
.remove_data_region <- function(batch_o = NULL) {
  .remove_data_by_batch_o("data_region", batch_o)
}

#' @noRd
.refresh_keywords <- function(x.type) {
  dt <- gt.env$dt_keywords[
    gt.env$dt_keywords$type == x.type,
    c("batch", "keyword")
  ]
  assign(paste0("keywords_", x.type), as.data.frame(dt), envir = gt.env)
  invisible(TRUE)
}

#' @noRd
.refresh_time <- function(x.type) {
  dt <- gt.env$dt_time[
    gt.env$dt_time$type == x.type,
    c("batch", "start_date", "end_date")
  ]
  assign(paste0("time_", x.type), as.data.frame(dt), envir = gt.env)
  invisible(TRUE)
}
