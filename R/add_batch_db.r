#' @title Add keyword batches to data_con and data_obj
#'
#' @description
#' @details
#'
#' @param type Type batch that should be added. Object of class \code{character}
#'  of value "control" or "object".
#' @param keyword Keywords that should be added as batch. Vector of class
#' \code{character} containing a maximum of five keywords.
#' @param time Time frame for which the batch data should be loaded. Object of
#' class \code{character} that takes the from "YYYY-MM-DD YYYY-MM-DD".
#'
#' @seealso
#' @return
#' Message that the batch was created successfully. Batch data is
#' uploaded to batch_terms and batch_time.
#'
#' @examples
#' \dontrun{
#' add_batch(
#'   type = "control", keyword = c("gmail", "youtube"), time =
#'     "2016-01-01 2019-12-31"
#' )
#' add_batch(
#'   type = "object", keyword = c("facebook", "google"), time =
#'     "2016-01-01 2019-12-31"
#' )
#' }
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom stringr str_c
#' @importFrom tibble tibble

add_batch <- function(type, keyword, time = "2010-01-01 2020-07-31") {
  if (length(keyword) > 5) error("'keyword' allows a maxium of five elements.\nYou supplied more than five elements.")
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
  } else {
    error("'type' allows only 'control' or 'object'.\nYuo supplied another value.")
  }
}
