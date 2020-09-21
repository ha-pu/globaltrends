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
#' remove_data(table = "data_mapping", control = 1, object = 1)
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
        .remove_batch_keywords(type = "control", batch = control)
      } else if (!is.null(object) & is.null(control)) {
        .remove_batch_keywords(type = "object", batch = object)
      }
    } else if (table == "batch_time") {
      if (!is.null(control) & is.null(object)) {
        .remove_batch_time(type = "control", batch = control)
      } else if (!is.null(object) & is.null(control)) {
        .remove_batch_time(type = "object", batch = object)
      }
    } else if (table == "data_control") {
      if (!is.null(control) & is.null(object)) {
        .remove_data_control(batch = control)
      }
    } else if (table == "data_object") {
      if (!is.null(object) & is.null(control)) {
        .remove_data_object(batch = object)
      }
    } else if (table == "data_mapping") {
      if (!is.null(control) | !is.null(object)) {
        .remove_data_mapping(batch_c = control, batch_o = object)
      }
    } else if (table == "data_score") {
      if (!is.null(control) | !is.null(object)) {
        .remove_data_score(batch_c = control, batch_o = object)
      }
    } else if (table == "data_doi") {
      if (!is.null(control) | !is.null(object)) {
        .remove_data_doi(batch_c = control, batch_o = object)
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

#' @rdname dot-remove_data
#' @title Remove from batch_keywords
#' @keywords internal

.remove_batch_keywords <- function(type, batch) {
  .test_batch(batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM batch_keywords WHERE type=? AND batch=?", params = list(type, batch))
  if (type == "control") {
    keywords_control <- filter(batch_keywords, type == "control")
    keywords_control <- select(keywords_control, -type)
    keywords_control <- collect(keywords_control)
	lst_export <- list(keywords_control, keywords_control)
	names(lst_export) <- list("keywords_control", ".keywords_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted control batch {batch} from 'batch_keywords'."))

    .remove_data_control(batch = batch)
  } else if (type == "object") {
    keywords_object <- filter(batch_keywords, type == "object")
    keywords_object <- select(keywords_object, -type)
    keywords_object <- collect(keywords_object)
	lst_export <- list(keywords_object, keywords_object)
	names(lst_export) <- list("keywords_object", ".keywords_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted object batch {batch} from 'batch_keywords'."))

    .remove_data_object(batch = batch)
  }

  .remove_batch_time(type = type, batch = batch)
}

#' @rdname dot-remove_data
#' @title Remove from batch_time
#' @keywords internal

.remove_batch_time <- function(type, batch) {
  .test_batch(batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM batch_time WHERE type=? AND batch=?", params = list(type, batch))
  if (type == "control") {
    time_control <- filter(batch_time, type == "control")
    time_control <- select(time_control, -type)
    time_control <- collect(time_control)
	lst_export <- list(time_control, time_control)
	names(lst_export) <- list("time_control", ".time_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted control batch {batch} from 'batch_time'."))
  } else if (type == "object") {
    time_object <- filter(batch_time, type == "object")
    time_object <- select(time_object, -type)
    time_object <- collect(time_object)
	lst_export <- list(time_object, time_object)
	names(lst_export) <- list("time_object", ".time_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully deleted object batch {batch} from 'batch_time'."))
  }
}

#' @rdname dot-remove_data
#' @title Remove from data_control
#' @keywords internal

.remove_data_control <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_control WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted control batch {batch} from 'data_control'."))

  .remove_data_mapping(batch_c = batch)
}

#' @rdname dot-remove_data
#' @title Remove from data_object
#' @keywords internal

.remove_data_object <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_object WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted object batch {batch} from 'data_object'."))

  .remove_data_mapping(batch_o = batch)
  .remove_data_global(batch = batch)
}

#' @rdname dot-remove_data
#' @title Remove from data_mapping
#' @keywords internal

.remove_data_mapping <- function(batch_c = NULL, batch_o = NULL) {
  if (is.null(batch_o) & !is.null(batch_c)) {
    .test_batch(batch_c)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_mapping WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_mapping'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_mapping WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_mapping'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_mapping WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_mapping'."))
  }

  .remove_data_score(batch_c = batch_c, batch_o = batch_o)
}

#' @rdname dot-remove_data
#' @title Remove from data_score
#' @keywords internal

.remove_data_score <- function(batch_c = NULL, batch_o = NULL) {
  if (is.null(batch_o) & !is.null(batch_c)) {
    .test_batch(batch_c)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_score'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_score'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_score'."))
  }

  .remove_data_doi(batch_c = batch_c, batch_o = batch_o)
}

#' @rdname dot-remove_data
#' @title Remove from data_doi
#' @keywords internal

.remove_data_doi <- function(batch_c = NULL, batch_o = NULL) {
  if (is.null(batch_o) & !is.null(batch_c)) {
    .test_batch(batch_c)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_doi WHERE batch_c=?", params = list(batch_c))
    message(glue("Successfully deleted control batch {batch_c} from 'data_doi'."))
  } else if (is.null(batch_c) & !is.null(batch_o)) {
    .test_batch(batch_o)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_doi WHERE batch_o=?", params = list(batch_o))
    message(glue("Successfully deleted object batch {batch_o} from 'data_doi'."))
  } else if (!is.null(batch_o) & !is.null(batch_c)) {
    walk(c(batch_c, batch_o), .test_batch)
    dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_doi WHERE batch_o=? AND batch_c=?", params = list(batch_c, batch_o))
    message(glue("Successfully deleted control batch {batch_c} and object batch {batch_o} from 'data_doi'."))
  }
}

#' @rdname dot-remove_data
#' @title Remove from data_global
#' @keywords internal

.remove_data_global <- function(batch) {
  .test_batch(batch)
  dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_global WHERE batch=?", params = list(batch))
  message(glue("Successfully deleted object batch {batch} from 'data_global'."))
}
