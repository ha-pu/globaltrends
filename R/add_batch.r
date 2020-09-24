#' @title Add batch of control or object keywords
#'
#' @param keyword Keywords that should be added as batch. Vector of class
#' \code{character} or a \code{list} of \code{character} objects. For control
#' keywords, batches can consist of up to five keywords, for object keywords
#' batch length is limited to four keyowrds. When a \code{character} vector
#' contains more than four (five) keywords, the vector is split into
#' four-keyword (five-keyword) batches. A \code{list} must contain
#' \code{character} objects of length four (five) or less.
#' @param time Time frame for which the batch data should be loaded. Object of
#' class \code{character} that takes the from "YYYY-MM-DD YYYY-MM-DD". Defaults
#' to "2010-01-01 2019-12-31".
#'
#' @return
#' Message that the batch was created successfully. Batch data is
#' uploaded to batch_keywords and batch_time.
#' @examples
#' \dontrun{
#' add_control_keyword(
#'   keyword = c("gmail", "maps", "translate", "wikipedia", "youtube"),
#'   time = "2016-01-01 2019-12-31"
#' )
#' add_object_keyword(
#'   keyword = c("apple", "facebook", "google", "microsoft"),
#'   time = "2016-01-01 2019-12-31"
#' )
#'
#' add_control_keyword(
#'   keyword = c("gmail", "maps", "news", "translate", "weather", "wikipedia", "youtube"),
#'   time = "2016-01-01 2019-12-31"
#' )
#' add_control_keyword(
#'   keyword = c("amazon", "apple", "facebook", "google", "microsoft", "netflix", "twitter"),
#'   time = "2016-01-01 2019-12-31"
#' )
#'
#' add_control_keyword(
#'   keyword = list(
#'     c("gmail", "maps", "news"),
#'     c("translate", "weather", "wikipedia", "youtube")
#'   ),
#'   time = "2016-01-01 2019-12-31"
#' )
#' add_control_keyword(
#'   keyword = list(
#'     c("amazon", "apple", "facebook", "google"),
#'     c("microsoft", "netflix", "twitter")
#'   ),
#'   time = "2016-01-01 2019-12-31"
#' )
#' }
#' @rdname add_batch
#' @export

add_control_keyword <- function(keyword, time = "2010-01-01 2019-12-31") {
  out <- .add_batch(type = "control", keyword = keyword, time = time, max = 5)
  return(out)
}

#' @title Add batch of object keywords
#'
#' @rdname add_batch
#' @export

add_object_keyword <- function(keyword, time = "2010-01-01 2019-12-31") {
  out <- .add_batch(type = "object", keyword = keyword, time = time, max = 4)
  return(out)
}

#' @title Add keyword batches to batch_keywords and batch_time
#'
#' @keywords internal
#'
#' @aliases
#' .add_batch
#' .add_batch.character
#' .add_batch.list
#'
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom tibble tibble

.add_batch <- function(type, keyword, time = "2010-01-01 2019-12-31", max) UseMethod(".add_batch", keyword)

#' @keywords internal
#' @rdname add_batch
#' @method .add_batch character

.add_batch.character <- function(type, keyword, time = "2010-01-01 2019-12-31", max) {
  if (length(keyword) > max) {
    keyword <- split(keyword, ceiling(seq_along(keyword) / max))
  } else {
    keyword <- list(keyword)
  }
  new_batches <- map(keyword, ~ .add_keyword_batch(type = type, keyword = .x, time = time, max = max))
  new_batches <- unname(new_batches)
  return(new_batches)
}

#' @keywords internal
#' @rdname add_batch
#' @method .add_batch list

.add_batch.list <- function(type, keyword, time = "2010-01-01 2019-12-31", max) {
  new_batches <- map(keyword, ~ .add_keyword_batch(type = type, keyword = .x, time = time, max = max))
  new_batches <- unname(new_batches)
  return(new_batches)
}

#' @title Add batch of keywords
#' @keywords internal

.add_keyword_batch <- function(type, keyword, time, max) {
  if (length(keyword) > max) stop(glue("Error: Length of list objects must not exceed {max}.\nYou provided a list object with length {length(keyword)}."))
  if (!is.character(keyword)) stop(glue("Error: 'keyword' must be object of type character.\nYou provided an object of type {typeof(keyword)}."))
  if (length(time) > 1) stop(glue("Error: 'time' must be object of length 1.\nYou provided an object of length {length(time)}."))
  if (!is.character(time)) stop(glue("Error: 'time' must be object of type character.\nYou provided an object of type {typeof(time)}."))
  if (type == "control") {
    if (nrow(.keywords_control) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(.keywords_control$batch) + 1
    }
    data <- tibble(batch = new_batch, keyword, type = "control")
    dbWriteTable(conn = globaltrends_db, name = "batch_keywords", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "control")
    dbWriteTable(conn = globaltrends_db, name = "batch_time", value = data, append = TRUE)
    keywords_control <- filter(.tbl_keywords, type == "control")
    keywords_control <- select(keywords_control, -type)
    keywords_control <- collect(keywords_control)
    lst_export <- list(keywords_control, keywords_control)
    names(lst_export) <- list("keywords_control", ".keywords_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    time_control <- filter(.tbl_time, type == "control")
    time_control <- select(time_control, -type)
    time_control <- collect(time_control)
    lst_export <- list(time_control, time_control)
    names(lst_export) <- list("time_control", ".time_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully created new control batch {new_batch} ({keyword_collapse}, {time}).", keyword_collapse = paste(keyword, collapse = ", ")))
    return(new_batch)
  } else if (type == "object") {
    if (nrow(.keywords_object) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(.keywords_object$batch) + 1
    }
    data <- tibble(batch = new_batch, keyword, type = "object")
    dbWriteTable(conn = globaltrends_db, name = "batch_keywords", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "object")
    dbWriteTable(conn = globaltrends_db, name = "batch_time", value = data, append = TRUE)
    keywords_object <- filter(.tbl_keywords, type == "object")
    keywords_object <- select(keywords_object, -type)
    keywords_object <- collect(keywords_object)
    lst_export <- list(keywords_object, keywords_object)
    names(lst_export) <- list("keywords_object", ".keywords_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    time_object <- filter(.tbl_time, type == "object")
    time_object <- select(time_object, -type)
    time_object <- collect(time_object)
    lst_export <- list(time_object, time_object)
    names(lst_export) <- list("time_object", ".time_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully created new object batch {new_batch} ({keyword_collapse}, {time}).", keyword_collapse = paste(keyword, collapse = ", ")))
    return(new_batch)
  } else {
    stop("Error: 'type' allows only 'control' or 'object'.\nYou supplied another value.")
  }
}
