#' @title Remove data from database tables
#'
#' @description
#' @details
#'
#' @param table Database table from which the batch should be removed.  Object
#' of class \code{character}.
#' @param batch_c Control batch for which the data is removed Object
#' of class \code{numeric}.
#' @param batch_o Object batch for which the data is removed Object
#' of class \code{numeric}.
#'
#' @seealso
#'
#' @return Message that data was removed successfully. Data is removed
#' from database.
#'
#' @examples
#' \dontrun{
#' run_remove(table = "batch_terms", batch_c = 1)
#' run_remove(table = "data_map", batch_c = 1, batch_o = 1)
#' }
#'
#' @export
#' @importFrom DBI dbExecute
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom glue glue

run_remove <- function(table, batch_c = NULL, batch_o = NULL) {
  if (is.character(table)) {
    if (table == "batch_terms") {
      if (!is.null(batch_c) & is.null(batch_o)) {
        .remove_batch_terms(type = "con", batch = batch_c)
      } else if (!is.null(batch_o) & is.null(batch_c)) {
        .remove_batch_terms(type = "obj", batch = batch_o)
      }
    } else if (table == "batch_time") {
      if (!is.null(batch_c) & is.null(batch_o)) {
        .remove_batch_time(type = "con", batch = batch_c)
      } else if (!is.null(batch_o) & is.null(batch_c)) {
        .remove_batch_time(type = "obj", batch = batch_o)
      }
    } else if (table == "data_con") {
      if (!is.null(batch_c) & is.null(batch_o)) {
        .remove_data_con(batch = batch_c)
      }
    } else if (table == "data_obj") {
      if (!is.null(batch_o) & is.null(batch_c)) {
        .remove_data_obj(batch = batch_o)
      }
    } else if (table == "data_map") {
      if (!is.null(batch_c) | !is.null(batch_o)) {
        .remove_data_map(batch_c = batch_c, batch_o = batch_o)
      }
    } else if (table == "data_score") {
      if (!is.null(batch_c) | !is.null(batch_o)) {
        .remove_data_score(batch_c = batch_c, batch_o = batch_o)
      }
    } else if (table == "data_agg") {
      if (!is.null(batch_c) | !is.null(batch_o)) {
        .remove_data_agg(batch_c = batch_c, batch_o = batch_o)
      }
    } else if (table == "data_wrld") {
      if (!is.null(batch_o) & is.null(batch_c)) {
        .remove_data_wrld(batch = batch_o)
      }
    }
  } else {
    stop("Error: 'table' must be an object of type character.\nYou supplied an object of a different type.")
  }
}

#' @title Remove from batch_terms
#' @keywords internal

.remove_batch_terms <- function(type, batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM batch_terms WHERE type=? AND batch=?", params = list(type, batch))
  if (type == "con") {
    terms_con <- filter(batch_terms, type == "con")
    terms_con <- collect(terms_con)
    assign("terms_con", terms_con, envir = .GlobalEnv)
    message(glue("Successfully deleted control batch {batch} from 'batch_terms'."))

    .remove_data_con(batch = batch)
  } else if (type == "obj") {
    terms_obj <- filter(batch_terms, type == "obj")
    terms_obj <- collect(terms_obj)
    assign("terms_obj", terms_obj, envir = .GlobalEnv)
    message(glue("Successfully deleted object batch {batch} from 'batch_terms'."))

    .remove_data_obj(batch = batch)
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

#' @title Remove from data_con
#' @keywords internal

.remove_data_con <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_con WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted control batch {batch} from 'data_con'."))

  .remove_data_map(batch_c = batch)
}

#' @title Remove from data_obj
#' @keywords internal

.remove_data_obj <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_obj WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted object batch {batch} from 'data_obj'."))

  .remove_data_map(batch_o = batch)
  .remove_data_wrld(batch = batch)
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
    message(glue("Control batch {batch_c} deleted from 'data_score'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_score WHERE batch_o=?", params = list(batch_o))
    message(glue("object batch {batch_o} deleted from 'data_score'."))
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
    message(glue("Control batch {batch_c} deleted from 'data_agg'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_agg WHERE batch_o=?", params = list(batch_o))
    message(glue("object batch {batch_o} deleted from 'data_agg'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_agg WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_agg'."))
  }
}

#' @title Remove from data_wrld
#' @keywords internal

.remove_data_wrld <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = doiGT_DB, statement = "DELETE FROM data_wrld WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted object batch {batch} from 'data_wrld'."))
}
