#' @title Remove data from database tables
#'
#' @description
#' The function removes data from database tables for control or object batches.
#'
#' @details
#' The function removes data "greedily": all data that builds on the deleted
#' data is removed. For example, when data from \emph{data_control} is removed
#' data in \emph{data_object} that maps to this control batch is also removed.
#' The dependency structure works as follows: \emph{batch_keyword} /
#' \emph{batch_time} -> \emph{data_control} -> \emph{data_object} ->
#' \emph{data_score} -> \emph{data_doi}.
#'
#' @param table Database table from which the batch should be removed.  Object
#' of type \code{character}.
#' @param control Control batch for which the data is removed Object
#' of type \code{numeric}.
#' @param object Object batch for which the data is removed Object
#' of type \code{numeric}.
#'
#' @seealso
#' * \code{\link{batch_keywords}}
#' * \code{\link{batch_time}}
#' * \code{\link{data_control}}
#' * \code{\link{data_object}}
#' * \code{\link{data_score}}
#' * \code{\link{data_doi}}
#'
#' @return Message that data has been removed successfully. Data is removed
#' from database tables.
#'
#' @examples
#' \dontrun{
#' remove_data(
#'   table = "batch_keywords",
#'   control = 1
#' )
#' remove_data(
#'   table = "data_score",
#'   control = 1,
#'   object = 1
#' )
#' }
#'
#' @export
#' @importFrom DBI dbExecute
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom rlang .data

remove_data <- function(table, control = NULL, object = NULL) {
  .check_length(table, 1)
  .check_length(control, 1)
  .check_length(object, 1)
  .check_input(table, "character")
  if (table == "batch_keywords") {
    if (!is.null(control) & is.null(object)) {
      .remove_batch_keywords(type = "control", batch_c = control, batch_o = object)
    } else if (!is.null(object) & is.null(control)) {
      .remove_batch_keywords(type = "object", batch_c = control, batch_o = object)
    }
  } else if (table == "batch_time") {
    if (!is.null(control) & is.null(object)) {
      .remove_batch_time(type = "control", batch_c = control, batch_o = object)
    } else if (!is.null(object) & is.null(control)) {
      .remove_batch_time(type = "object", batch_c = control, batch_o = object)
    }
  } else if (table == "data_control") {
    if (!is.null(control)) {
      .remove_data_control(batch_c = control, batch_o = object)
    }
  } else if (table == "data_object") {
    if (!is.null(object) | !is.null(control)) {
      .remove_data_object(batch_c = control, batch_o = object)
    }
  } else if (table == "data_score") {
    if (!is.null(control) | !is.null(object)) {
      .remove_data_score(batch_c = control, batch_o = object)
    }
  } else if (table == "data_doi") {
    if (!is.null(control) | !is.null(object)) {
      .remove_data_doi(batch_c = control, batch_o = object)
    }
  } else {
    stop(glue("Error: 'table' must be either 'batch_keywords', 'batch_time', 'data_control', 'data_object', 'data_score', or 'data_doi'.\nYou provided {table}."))
  }
}

#' @title Remove from batch_keywords
#' @keywords internal
#' @noRd

.remove_batch_keywords <- function(type, batch_c, batch_o) {
  walk(list(batch_c, batch_o), .check_batch)
  if (type == "control") {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM batch_keywords WHERE type=? AND batch=?", params = list(type, batch_c))
    keywords_control <- filter(batch_keywords, .data$type == "control")
    keywords_control <- select(keywords_control, -.data$type)
    keywords_control <- collect(keywords_control)
    lst_export <- list(keywords_control, keywords_control)
    names(lst_export) <- list("keywords_control", ".keywords_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted control batch {batch_c} from 'batch_keywords'."))

    .remove_data_control(batch_c = batch_c)
  } else if (type == "object") {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM batch_keywords WHERE type=? AND batch=?", params = list(type, batch_o))
    keywords_object <- filter(batch_keywords, .data$type == "object")
    keywords_object <- select(keywords_object, -.data$type)
    keywords_object <- collect(keywords_object)
    lst_export <- list(keywords_object, keywords_object)
    names(lst_export) <- list("keywords_object", ".keywords_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted object batch {batch_o} from 'batch_keywords'."))

    .remove_data_object(batch_o = batch_o)
  }

  .remove_batch_time(type = type, batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from batch_time
#' @keywords internal
#' @noRd

.remove_batch_time <- function(type, batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch)
  if (type == "control") {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM batch_time WHERE type=? AND batch=?", params = list(type, batch_c))
    time_control <- filter(batch_time, .data$type == "control")
    time_control <- select(time_control, -.data$type)
    time_control <- collect(time_control)
    lst_export <- list(time_control, time_control)
    names(lst_export) <- list("time_control", ".time_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted control batch {batch_c} from 'batch_time'."))
  } else if (type == "object") {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM batch_time WHERE type=? AND batch=?", params = list(type, batch_o))
    time_object <- filter(batch_time, .data$type == "object")
    time_object <- select(time_object, -.data$type)
    time_object <- collect(time_object)
    lst_export <- list(time_object, time_object)
    names(lst_export) <- list("time_object", ".time_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted object batch {batch_o} from 'batch_time'."))
  }
}

#' @title Remove from data_control
#' @keywords internal
#' @noRd

.remove_data_control <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_control WHERE batch=?", params = list(batch_c))
  message(glue("Successfully deleted control batch {batch_c} from 'data_control'."))

  .remove_data_object(batch_c = batch_c)
}

#' @title Remove from data_object
#' @keywords internal
#' @noRd

.remove_data_object <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch)
  if (is.null(batch_o) & !is.null(batch_c)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_object WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_object'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_object WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_object'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_object WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_object'."))
  }

  .remove_data_score(batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from data_score
#' @keywords internal
#' @noRd

.remove_data_score <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch)
  if (is.null(batch_o) & !is.null(batch_c)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_score'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_score'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_score'."))
  }

  .remove_data_doi(batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from data_doi
#' @keywords internal
#' @noRd

.remove_data_doi <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch)
  if (is.null(batch_o) & !is.null(batch_c)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_doi WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_doi'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_doi WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_doi'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_doi WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_doi'."))
  }
}

#' @title Remove from data_global
#' @keywords internal
#' @noRd

.remove_data_global <- function(batch_c = NULL, batch_o = NULL) {
  walk(list(batch_c, batch_o), .check_batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_global WHERE batch=?", params = list(batch_o))
  message(glue("Successfully deleted object batch {batch_o} from 'data_global'."))
}
