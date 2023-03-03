#' @title Add batches of control or object keywords
#'
#' @description
#' The function adds one or more batches of keywords with a time period for
#' downloads to the database. The batches serve as input for all download and
#' computation functions.
#'
#' @details
#' Since Google Trends allows a maximum of five keywords for each query, batches
#' of control keywords can consist of up to five keywords. Since one control
#' keyword is added to batches of object keywords for mapping, object batch
#' length is limited to four keywords. When a `character` vector contains
#' more than four (five) keywords, the vector is split into four-keyword
#' (five-keyword) batches. A `list` must contain `character` vectors
#' of length four (five) or less. Each batch of keywords is combined with a time
#' period for which data will be downloaded. To change the time period for an
#' existing batch, all downloads and computations must be rerun.
#'
#' @section Warning:
#' If you use search topics for object keywords, make sure to use search topics
#' for control keywords and vice versa. See Google's
#' [FAQ](https://web.archive.org/web/20230117193147/https://support.google.com/trends/answer/4359550/)
#' for additional information on search topics.
#'
#' @section Note:
#' To avoid trailing spaces `stringr::str_squish` is automatically
#' applied to all keywords.
#'
#' @param keyword Keywords that should be added as batch. Vector of type
#' `character` or a `list` of `character` vectors. The function also allows the
#' usage of codes for search topics instead of search terms.
#'
#' @param time Time frame for which the batch data should be downloaded. Object
#' of type `character` that takes the from "YYYY-MM-DD YYYY-MM-DD".
#' Defaults to *"2010-01-01 2020-12-31"*.
#'
#' @return
#' Message that the batch has been created successfully. Batch data is
#' written to tables *batch_keywords* and *batch_time*.
#' Numeric vector containing the newly added batch numbers are returned.
#'
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
#'
#' # search topics
#' add_control_keyword(
#'   keyword = c("%2Fm%2F02q_bk", "%2Fm%2F055t58", "%2Fm%2F025sndk", "%2Fm%2F0d07ph", "%2Fm%2F09jcvs"),
#'   time = "2016-01-01 2019-12-31"
#' )
#' # This adds the following topics: Gmail, Google Maps, Google Translate, Wikipedia, YouTube
#' }
#'
#' @seealso
#' * [example_keywords()]
#' * [example_time()]
#' * [stringr::str_squish()]
#'
#' @rdname add_keyword
#' @export

add_control_keyword <- function(keyword, time = "2010-01-01 2020-12-31") {
  out <- .add_batch(type = "control", keyword = keyword, time = time, max = 5)
  return(out)
}

#' @title Add batch of object keywords
#'
#' @rdname add_keyword
#' @export

add_object_keyword <- function(keyword, time = "2010-01-01 2020-12-31") {
  out <- .add_batch(type = "object", keyword = keyword, time = time, max = 4)
  return(out)
}

#' @title Add keyword batches to batch_keywords and batch_time
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom stringr str_squish
#' @importFrom tibble tibble

.add_batch <- function(type, keyword, time, max) UseMethod(".add_batch", keyword)

#' @keywords internal
#' @noRd

.add_batch.character <- function(type, keyword, time, max) {
  if (length(keyword) > max) {
    keyword <- split(keyword, ceiling(seq_along(keyword) / max))
  } else {
    keyword <- list(keyword)
  }
  new_batches <- map_dbl(keyword, ~ .add_keyword_batch(type = type, keyword = .x, time = time, max = max))
  new_batches <- unname(new_batches)
  return(new_batches)
}

#' @keywords internal
#' @noRd

.add_batch.list <- function(type, keyword, time, max) {
  new_batches <- map_dbl(keyword, ~ .add_keyword_batch(type = type, keyword = .x, time = time, max = max))
  new_batches <- unname(new_batches)
  return(new_batches)
}

#' @title Add batch of keywords
#' @keywords internal
#' @noRd

.add_keyword_batch <- function(type, keyword, time, max) {
  .check_length(keyword, max)
  .check_input(keyword, "character")
  .check_length(time, 1)
  .check_input(time, "character")
  if (type == "control") {
    if (nrow(gt.env$keywords_control) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(gt.env$keywords_control$batch) + 1
    }
    keyword <- str_squish(keyword)
    data <- tibble(batch = new_batch, keyword, type = "control")
    dbAppendTable(conn = gt.env$globaltrends_db, name = "batch_keywords", value = data)
    data <- tibble(batch = new_batch, time = time, type = "control")
    dbAppendTable(conn = gt.env$globaltrends_db, name = "batch_time", value = data)
    keywords_control <- filter(gt.env$tbl_keywords, .data$type == "control")
    keywords_control <- select(keywords_control, -type)
    keywords_control <- collect(keywords_control)
    lst_export <- list(keywords_control, keywords_control)
    names(lst_export) <- list("keywords_control", "keywords_control")
    invisible(list2env(lst_export, envir = gt.env))
    time_control <- filter(gt.env$tbl_time, .data$type == "control")
    time_control <- select(time_control, -type)
    time_control <- collect(time_control)
    lst_export <- list(time_control, time_control)
    names(lst_export) <- list("time_control", "time_control")
    invisible(list2env(lst_export, envir = gt.env))
    message(paste0("Successfully created new control batch ", new_batch, " (", paste(keyword, collapse = ", "), ", ", time, ")."))
    return(new_batch)
  } else if (type == "object") {
    if (nrow(gt.env$keywords_object) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(gt.env$keywords_object$batch) + 1
    }
    data <- tibble(batch = new_batch, keyword, type = "object")
    dbAppendTable(conn = gt.env$globaltrends_db, name = "batch_keywords", value = data)
    data <- tibble(batch = new_batch, time = time, type = "object")
    dbAppendTable(conn = gt.env$globaltrends_db, name = "batch_time", value = data)
    keywords_object <- filter(gt.env$tbl_keywords, .data$type == "object")
    keywords_object <- select(keywords_object, -type)
    keywords_object <- collect(keywords_object)
    lst_export <- list(keywords_object, keywords_object)
    names(lst_export) <- list("keywords_object", "keywords_object")
    invisible(list2env(lst_export, envir = gt.env))
    time_object <- filter(gt.env$tbl_time, .data$type == "object")
    time_object <- select(time_object, -type)
    time_object <- collect(time_object)
    lst_export <- list(time_object, time_object)
    names(lst_export) <- list("time_object", "time_object")
    invisible(list2env(lst_export, envir = gt.env))
    message(paste0("Successfully created new object batch ", new_batch, " (", paste(keyword, collapse = ", "), ", ", time, ")."))
    return(new_batch)
  } else {
    stop("Error: 'type' allows only 'control' or 'object'.\nYou provided another value.")
  }
}

#' Add synonyms for object keywords
#'
#' @aliases
#' add_synonym
#' add_synonym.character
#' add_synonym.list
#'
#' @description
#' The function allows to add synonyms for object keywords. Sometimes, objects
#' of interest can be searched with different keywords on Google e.g., FC Bayern
#' for Bayern Munich. Search scores for keywords that are added as synonyms are
#' aggregated when running `compute_score`. The function allows to add
#' synonyms for a single keyword at a time.
#'
#' @section Note:
#' To avoid trailing spaces `stringr::str_squish` is automatically
#' applied to all keywords and synonyms.
#'
#' @param keyword Keyword of type `character` and length 1 for which the
#' synonyms are added.
#'
#' @param synonym Synonym of type `character`.
#'
#' @return
#' Message that the synonym has been added successfully. Synonym data is
#' written to table *keyword_synonyms*.
#'
#' @seealso
#' * [compute_score()]
#' * [stringr::str_squish()]
#'
#' @examples
#' \dontrun{
#' add_synonym(
#'   keyword = "fc bayern",
#'   synonym = "bayern munich"
#' )
#' }
#'
#' @export
#' @rdname add_synonym
#' @importFrom DBI dbAppendTable
#' @importFrom purrr walk
#' @importFrom stringr str_squish
#' @importFrom tibble tibble

add_synonym <- function(keyword, synonym) UseMethod("add_synonym", synonym)

#' @export
#' @rdname add_synonym

add_synonym.character <- function(keyword, synonym) {
  .check_length(keyword, 1)
  .check_input(keyword, "character")
  if (length(synonym) > 1) {
    add_synonym(keyword = keyword, synonym = as.list(synonym))
  } else {
    if (!is.character(synonym)) stop(paste0("Error:'synonym' must of type 'character'.\nYou provided an object of type ", typeof(synonym), "."))
    keyword <- str_squish(keyword)
    synonym <- str_squish(synonym)
    out <- tibble(keyword, synonym)
    dbAppendTable(
      conn = gt.env$globaltrends_db,
      name = "keyword_synonyms",
      value = out,
      append = TRUE
    )
    keyword_synonyms <- collect(gt.env$tbl_synonyms)
    lst_export <- list(keyword_synonyms, keyword_synonyms)
    names(lst_export) <- list("keyword_synonyms", "keyword_synonyms")
    invisible(list2env(lst_export, envir = gt.env))
    message(paste0("Successfully added synonym | keyword: ", keyword, " | synonym: ", synonym, "."))
  }
}

#' @export
#' @rdname add_synonym

add_synonym.list <- function(keyword, synonym) {
  walk(synonym, add_synonym, keyword = keyword)
}
