#' @title Remove data from database tables
#'
#' @description
#' @details
#'
#' @param table Database table from which the batch should be removed.  Object
#' of class \code{character}.
#' @param control Control batch for which the data is removed Object
#' of class \code{numeric}.
#' @param object Object batch for which the data is removed Object
#' of class \code{numeric}.
#'
#' @seealso
#'
#' @return Message that data was removed successfully. Data is removed
#' from database.
#'
#' @examples
#' \dontrun{
#' remove_data(table = "batch_keywords", control = 1)
#' remove_data(table = "data_map", control = 1, object = 1)
#' }
#'
#' @export
#' @importFrom DBI dbExecute
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom glue glue

remove_data <- function(table, control = NULL, object = NULL) {
  control <- control[[1]]
  object <- object[[1]]
  if (is.character(table)) {
    if (table == "batch_keywords") {
      if (!is.null(control) & is.null(object)) {
        .remove_batch_keywords(type = "con", batch = control)
      } else if (!is.null(object) & is.null(control)) {
        .remove_batch_keywords(type = "obj", batch = object)
      }
    } else if (table == "batch_time") {
      if (!is.null(control) & is.null(object)) {
        .remove_batch_time(type = "con", batch = control)
      } else if (!is.null(object) & is.null(control)) {
        .remove_batch_time(type = "obj", batch = object)
      }
    } else if (table == "data_control") {
      if (!is.null(control) & is.null(object)) {
        .remove_data_control(batch = control)
      }
    } else if (table == "data_object") {
      if (!is.null(object) & is.null(control)) {
        .remove_data_object(batch = object)
      }
    } else if (table == "data_map") {
      if (!is.null(control) | !is.null(object)) {
        .remove_data_map(batch_c = control, batch_o = object)
      }
    } else if (table == "data_score") {
      if (!is.null(control) | !is.null(object)) {
        .remove_data_score(batch_c = control, batch_o = object)
      }
    } else if (table == "data_agg") {
      if (!is.null(control) | !is.null(object)) {
        .remove_data_agg(batch_c = control, batch_o = object)
      }
    } else if (table == "data_global") {
      if (!is.null(object) & is.null(control)) {
        .remove_data_global(batch = object)
      }
    }
  } else {
    stop("Error: 'table' must be an object of type character.\nYou supplied an object of a different type.")
  }
}

#' @title Remove from batch_keywords
#' @keywords internal

.remove_batch_keywords <- function(type, batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM batch_keywords WHERE type=? AND batch=?", params = list(type, batch))
  if (type == "con") {
    terms_con <- filter(batch_keywords, type == "con")
    terms_con <- collect(terms_con)
    assign("terms_con", terms_con, envir = .GlobalEnv)
    message(glue("Successfully deleted control batch {batch} from 'batch_keywords'."))

    .remove_data_control(batch = batch)
  } else if (type == "obj") {
    terms_obj <- filter(batch_keywords, type == "obj")
    terms_obj <- collect(terms_obj)
    assign("terms_obj", terms_obj, envir = .GlobalEnv)
    message(glue("Successfully deleted object batch {batch} from 'batch_keywords'."))

    .remove_data_object(batch = batch)
  }

  .remove_batch_time(type = type, batch = batch)
}

#' @title Remove from batch_time
#' @keywords internal

.remove_batch_time <- function(type, batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM batch_time WHERE type=? AND batch=?", params = list(type, batch))
  if (type == "con") {
    time_con <- filter(batch_time, type == "con")
    time_con <- collect(time_con)
    assign("time_con", time_con, envir = .GlobalEnv)
    message(glue("Successfully deleted control batch {batch} from 'batch_time'."))
  } else if (type == "obj") {
    time_obj <- filter(batch_time, type == "obj")
    time_obj <- collect(time_obj)
    assign("time_obj", time_obj, envir = .GlobalEnv)
    message(glue("Successfully deleted object batch {batch} from 'batch_time'."))
  }
}

#' @title Remove from data_control
#' @keywords internal

.remove_data_control <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_control WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted control batch {batch} from 'data_control'."))

  .remove_data_map(batch_c = batch)
}

#' @title Remove from data_object
#' @keywords internal

.remove_data_object <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_object WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted object batch {batch} from 'data_object'."))

  .remove_data_map(batch_o = batch)
  .remove_data_global(batch = batch)
}

#' @title Remove from data_map
#' @keywords internal

.remove_data_map <- function(batch_c = NULL, batch_o = NULL) {
  if (is.null(batch_o) & !is.null(batch_c)) {
    .test_batch(batch_c)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_map WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_map'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_map WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_map'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_map WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_map'."))
  }

  .remove_data_score(batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from data_score
#' @keywords internal

.remove_data_score <- function(batch_c = NULL, batch_o = NULL) {
  if (is.null(batch_o) & !is.null(batch_c)) {
    .test_batch(batch_c)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_score WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_score'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_score WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_score'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_score WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_score'."))
  }

  .remove_data_agg(batch_c = batch_c, batch_o = batch_o)
}

#' @title Remove from data_agg
#' @keywords internal

.remove_data_agg <- function(batch_c = NULL, batch_o = NULL) {
  if (is.null(batch_o) & !is.null(batch_c)) {
    .test_batch(batch_c)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_agg WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_agg'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_agg WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_agg'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_agg WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_agg'."))
  }
}

#' @title Remove from data_global
#' @keywords internal

.remove_data_global <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_global WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted object batch {batch} from 'data_global'."))
}
