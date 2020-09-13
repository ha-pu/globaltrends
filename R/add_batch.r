#' @title Add batch of control keywords
#' 
#' @param keyword Keywords that should be added as batch. Vector of class
#' \code{character} or a \code{list} of \code{character} element. When a
#' \code{character} vector contains more than five keywords, the vector is
#' split into five-keyword batches. A \code{list} must contain
#' \code{character} elements of length five or less.
#' @param time Time frame for which the batch data should be loaded. Object of
#' class \code{character} that takes the from "YYYY-MM-DD YYYY-MM-DD".
#' 
#' @return
#' Message that the batch was created successfully. Batch data is
#' uploaded to batch_terms and batch_time.
#' @examples
#' \dontrun{
#' add_control_keyword(
#'   keyword = c("gmail", "maps", "translate", "wikipedia", "youtube"),
#'   time = "2016-01-01 2019-12-31"
#' )
#' add_control_keyword(
#'   keyword = c("gmail", "maps", "news", "translate", "weather", "wikipedia",
#'   "youtube"), time = "2016-01-01 2019-12-31"
#' )
#' add_control_keyword(
#'   keyword = list(c("gmail", "maps", "news"), c("translate", "weather",
#'   "wikipedia", "youtube")), time = "2016-01-01 2019-12-31"
#' )
#' }
#' @rdname add_batch
#' @export

add_control_keyword <- function(keyword, time = "2010-01-01 2020-07-31") {
  out <- .add_batch(type = "control", keyword = keyword, time = time)
  return(out)
}
  
#' @title Add batch of object keywords
#' 
#' @param keyword Keywords that should be added as batch. Vector of class
#' \code{character} or a \code{list} of \code{character} element. When a
#' \code{character} vector contains more than five keywords, the vector is
#' split into five-keyword batches. A \code{list} must contain
#' \code{character} elements of length five or less.
#' @param time Time frame for which the batch data should be loaded. Object of
#' class \code{character} that takes the from "YYYY-MM-DD YYYY-MM-DD".
#' 
#' @return
#' Message that the batch was created successfully. Batch data is
#' uploaded to batch_terms and batch_time.
#'
#' @examples
#' \dontrun{
#' add_control_keyword(
#'   keyword = c("amazon", "apple", "facebook", "google", "microsoft"),
#'   time = "2016-01-01 2019-12-31"
#' )
#' add_control_keyword(
#'   keyword = c("amazon", "apple", "facebook", "google", "microsoft",
#'   "netflix", "twitter"), time = "2016-01-01 2019-12-31"
#' )
#' add_control_keyword(
#'   keyword = list(c("amazon", "apple", "facebook", "google")), c("microsoft",
#'   "netflix", "twitter")), time = "2016-01-01 2019-12-31"
#' )
#' }
#' @rdname add_batch
#' @export

add_object_keyword <- function(keyword, time = "2010-01-01 2020-07-31") {
  out <- .add_batch(type = "object", keyword = keyword, time = time)
  return(out)
}

#' @title Add keyword batches to batch_terms and batch_time
#' 
#' @keywords internal
#' 
#' @aliases
#' .add_batch
#' .add_batch.character
#' .add_batch.list
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom stringr str_c
#' @importFrom tibble tibble

.add_batch <- function(type, keyword, time = "2010-01-01 2020-07-31") UseMethod(".add_batch", keyword)

#' @keywords internal
#' @rdname add_batch
#' @method add_batch character
#' @export

.add_batch.character <- function(type, keyword, time = "2010-01-01 2020-07-31") {
  if (length(keyword) > 5) {
    keyword <- split(keyword, ceiling(seq_along(keyword) / 5))
  } else {
    keyword <- list(keyword)
  }
  new_batches <- map(keyword, ~ .add_keyword_batch(type = type, keyword = .x, time = time))
  new_batches <- unname(new_batches)
  return(new_batches)
}

#' @keywords internal
#' @rdname add_batch
#' @method add_batch list
#' @export

.add_batch.list <- function(type, keyword, time = "2010-01-01 2020-07-31") {
  new_batches <- map(keyword, ~ .add_keyword_batch(type = type, keyword = .x, time = time))
  new_batches <- unname(new_batches)
  return(new_batches)
}

#' @title Add batch of keywords
#' @keywords internal

.add_keyword_batch <- function(type, keyword, time) {
  if (length(keyword) > 5) stop("Error: Lenght of list elements must not exceed 5.\nYou provided a list elment with length > 5.")
  if (type == "control") {
    if (nrow(terms_con) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(terms_con$batch) + 1
    }
    data <- tibble(batch = new_batch, keyword, type = "con")
    dbWriteTable(conn = gtrends_db, name = "batch_terms", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "con")
    dbWriteTable(conn = gtrends_db, name = "batch_time", value = data, append = TRUE)
    terms_con <- filter(batch_terms, type == "con")
    terms_con <- collect(terms_con)
    assign("terms_con", terms_con, envir = .GlobalEnv)
    time_con <- filter(batch_time, type == "con")
    time_con <- collect(time_con)
    assign("time_con", time_con, envir = .GlobalEnv)
    message(str_c("New control batch", new_batch, "created.", sep = " "))
    return(new_batch)
  } else if (type == "object") {
    if (nrow(terms_obj) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(terms_obj$batch) + 1
    }
    data <- tibble(batch = new_batch, keyword, type = "obj")
    dbWriteTable(conn = gtrends_db, name = "batch_terms", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "obj")
    dbWriteTable(conn = gtrends_db, name = "batch_time", value = data, append = TRUE)
    terms_obj <- filter(batch_terms, type == "obj")
    terms_obj <- collect(terms_obj)
    assign("terms_obj", terms_obj, envir = .GlobalEnv)
    time_obj <- filter(batch_time, type == "obj")
    time_obj <- collect(time_obj)
    assign("time_obj", time_obj, envir = .GlobalEnv)
    message(str_c("New object batch", new_batch, "created.", sep = " "))
    return(new_batch)
  } else {
    stop("Error: 'type' allows only 'control' or 'object'.\nYuo supplied another value.")
  }
}
