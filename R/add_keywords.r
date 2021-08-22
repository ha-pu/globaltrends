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
#' @param keyword Keywords that should be added as batch. Vector of type
#' `character` or a `list` of `character` vectors.
#' @param time Time frame for which the batch data should be downloaded. Object
#' of type `character` that takes the from "YYYY-MM-DD YYYY-MM-DD".
#' Defaults to *"2010-01-01 2020-12-31"*.
#'
#' @return
#' Message that the batch has been created successfully. Batch data is
#' written to tables *batch_keywords* and *batch_time*.
#' Numeric vector containing the newly added batch numbers.
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
#' }
#' @seealso
#' * [example_keywords()]
#' * [example_time()]
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
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom rlang .data
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
    if (nrow(.keywords_control) == 0) {
      new_batch <- 1
    } else {
      new_batch <- max(.keywords_control$batch) + 1
    }
    data <- tibble(batch = new_batch, keyword, type = "control")
    dbWriteTable(conn = globaltrends_db, name = "batch_keywords", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "control")
    dbWriteTable(conn = globaltrends_db, name = "batch_time", value = data, append = TRUE)
    keywords_control <- filter(.tbl_keywords, .data$type == "control")
    keywords_control <- select(keywords_control, -.data$type)
    keywords_control <- collect(keywords_control)
    lst_export <- list(keywords_control, keywords_control)
    names(lst_export) <- list("keywords_control", ".keywords_control")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    time_control <- filter(.tbl_time, .data$type == "control")
    time_control <- select(time_control, -.data$type)
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
    keywords_object <- filter(.tbl_keywords, .data$type == "object")
    keywords_object <- select(keywords_object, -.data$type)
    keywords_object <- collect(keywords_object)
    lst_export <- list(keywords_object, keywords_object)
    names(lst_export) <- list("keywords_object", ".keywords_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    time_object <- filter(.tbl_time, .data$type == "object")
    time_object <- select(time_object, -.data$type)
    time_object <- collect(time_object)
    lst_export <- list(time_object, time_object)
    names(lst_export) <- list("time_object", ".time_object")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully created new object batch {new_batch} ({keyword_collapse}, {time}).", keyword_collapse = paste(keyword, collapse = ", ")))
    return(new_batch)
  } else {
    stop("Error: 'type' allows only 'control' or 'object'.\nYou provided another value.")
  }
}

#' Add synonyms for object keywords
#'
#' @description
#' The function allows to add synonyms for object keywords. Sometimes, objects
#' of interest can be searched with different keywords on Google e.g., FC Bayern
#' for Bayern Munich. Search scores for keywords that are added as synonyms are
#' aggregated when running `compute_score`. The function allows to add
#' synonyms for a single keyword at a time.
#'
#' @param keyword Keyword of type `character` and length 1 for which the
#' synonyms are added.
#' @param synonym Synonym of type `character`.
#'
#' @return
#' Message that the synonym has been added successfully. Synonym data is
#' written to table *keyword_synonyms*.
#'
#' @seealso
#' * [compute_score()]
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
#' @importFrom DBI dbWriteTable
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom tibble tibble

add_synonym <- function(keyword, synonym) UseMethod("add_synonym", synonym)

#' @export

add_synonym.character <- function(keyword, synonym) {
  .check_length(keyword, 1)
  .check_input(keyword, "character")
  if (length(synonym) > 1) {
    add_synonym(keyword = keyword, synonym = as.list(synonym))
  } else {
    if (!is.character(synonym)) stop(glue("Error:'synonym' must of type 'character'.\nYou provided an object of type {typeof(synonym)}."))
    out <- tibble(keyword, synonym)
    dbWriteTable(
      conn = globaltrends_db,
      name = "keyword_synonyms",
      value = out,
      append = TRUE
    )
    keyword_synonyms <- collect(.tbl_synonyms)
    lst_export <- list(keyword_synonyms, keyword_synonyms)
    names(lst_export) <- list("keyword_synonyms", ".keyword_synonyms")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully added synonym | keyword: {keyword} | synonym: {synonym}"))
  }
}

#' @export

add_synonym.list <- function(keyword, synonym) {
  walk(synonym, add_synonym, keyword = keyword)
}
