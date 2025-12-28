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
#' @param start_date Start of time frame for which the batch data should be downloaded. Object
#' of type `character` that takes the from "YYYY-MM".
#' Defaults to *"2010-01"*.
#'
#' @param end_date End of time frame for which the batch data should be downloaded. Object
#' of type `character` that takes the from "YYYY-MM".
#' Defaults to *"2020-12"*.
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
#'   start_date = "2016-01", end_date = "2019-12"
#' )
#' add_object_keyword(
#'   keyword = c("apple", "facebook", "google", "microsoft"),
#'   start_date = "2016-01", end_date = "2019-12"
#' )
#'
#' add_control_keyword(
#'   keyword = c("gmail", "maps", "news", "translate", "weather", "wikipedia", "youtube"),
#'   start_date = "2016-01", end_date = "2019-12"
#' )
#' add_control_keyword(
#'   keyword = c("amazon", "apple", "facebook", "google", "microsoft", "netflix", "twitter"),
#'   start_date = "2016-01", end_date = "2019-12"
#' )
#'
#' add_control_keyword(
#'   keyword = list(
#'     c("gmail", "maps", "news"),
#'     c("translate", "weather", "wikipedia", "youtube")
#'   ),
#'   start_date = "2016-01", end_date = "2019-12"
#' )
#' add_control_keyword(
#'   keyword = list(
#'     c("amazon", "apple", "facebook", "google"),
#'     c("microsoft", "netflix", "twitter")
#'   ),
#'   start_date = "2016-01", end_date = "2019-12"
#' )
#'
#' # search topics
#' add_control_keyword(
#'   keyword = c("%2Fm%2F02q_bk", "%2Fm%2F055t58", "%2Fm%2F025sndk", "%2Fm%2F0d07ph", "%2Fm%2F09jcvs"),
#'   start_date = "2016-01", end_date = "2019-12"
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

add_control_keyword <- function(
  keyword,
  start_date = "2010-01",
  end_date = "2020-12"
) {
  out <- .add_batch(
    type = "control",
    keyword = keyword,
    start_date = start_date,
    end_date = end_date,
    max = 5
  )
  return(out)
}

#' @title Add batch of object keywords
#'
#' @rdname add_keyword
#' @export

add_object_keyword <- function(
  keyword,
  start_date = "2010-01",
  end_date = "2020-12"
) {
  out <- .add_batch(
    type = "object",
    keyword = keyword,
    start_date = start_date,
    end_date = end_date,
    max = 4
  )
  return(out)
}

#' @title Add keyword batches to batch_keywords and batch_time
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom DBI dbAppendTable
#' @importFrom DBI dbWithTransaction
#' @importFrom purrr map_int
#' @importFrom rlang .data
#' @importFrom stringr str_squish
#' @importFrom tibble tibble

.add_batch <- function(type, keyword, start_date, end_date, max) {
  type <- match.arg(type, c("control", "object"))

  # ---- input validation (high level) ----
  .check_length(start_date, 1)
  .check_length(end_date, 1)
  .check_input(start_date, "character")
  .check_input(end_date, "character")
  .check_length(max, 1)

  if (!is.numeric(max) || is.na(max) || max < 1) {
    stop("`max` must be a positive integer.", call. = FALSE)
  }
  max <- as.integer(max)

  keyword_vec <- unlist(keyword, use.names = FALSE)
  keyword_vec <- as.character(keyword_vec)

  # Allow empty input to fail fast with a clear message.
  if (length(keyword_vec) == 0) {
    stop("`keyword` must contain at least one term.", call. = FALSE)
  }

  # ---- batching ----
  batches <- split(keyword_vec, ceiling(seq_along(keyword_vec) / max))
  n_batches <- length(batches)

  # Compute batch IDs deterministically for this call (avoids repeated cache reads)
  first_id <- .next_batch_id(type)
  batch_ids <- seq.int(first_id, length.out = n_batches)

  # ---- insert + refresh per batch ----
  map_int(seq_along(batches), function(i) {
    kw_batch <- str_squish(batches[[i]])
    .check_length(kw_batch, max)

    batch_id <- batch_ids[[i]]

    dbWithTransaction(gt.env$globaltrends_db, {
      # keywords table
      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "batch_keywords",
        value = tibble(
          batch = batch_id,
          keyword = kw_batch,
          type = type
        )
      )

      # time table
      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "batch_time",
        value = tibble(
          batch = batch_id,
          start_date = start_date,
          end_date = end_date,
          type = type
        )
      )
    })

    .refresh_cached_batches(type)

    message(sprintf(
      "Successfully created new %s batch %d (%s, %s-%s).",
      type,
      batch_id,
      paste(kw_batch, collapse = ", "),
      start_date,
      end_date
    ))

    batch_id
  })
}

# ---- helpers ----

#' @description
#' Determine the next available batch id for a given type based on the cached
#' data.Falls back safely if the cache object is missing or empty.
#'
#' @keywords internal
#' @noRd

.next_batch_id <- function(in_type) {
  cache_name <- paste0("keywords_", in_type)

  cache <- get0(cache_name, envir = gt.env, inherits = FALSE, ifnotfound = NULL)
  if (is.null(cache) || !is.data.frame(cache) || nrow(cache) == 0) {
    return(1L)
  }

  # Be defensive in case `batch` is not integer-typed.
  max_id <- suppressWarnings(max(as.integer(cache$batch), na.rm = TRUE))
  if (!is.finite(max_id)) 0L else (max_id + 1L)
}

#' @description
#' Refresh `keywords_<type>` and `time_<type>` from the DB-backed tbls in
#' `gt.env`.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr select

.refresh_cached_batches <- function(in_type) {
  kw_name <- paste0("keywords_", in_type)
  tm_name <- paste0("time_", in_type)

  keywords <- gt.env$tbl_keywords |>
    filter(.data$type == in_type) |>
    select(-type) |>
    collect()

  time <- gt.env$tbl_time |>
    filter(.data$type == in_type) |>
    select(-type) |>
    collect()

  assign(kw_name, keywords, envir = gt.env)
  assign(tm_name, time, envir = gt.env)

  invisible(NULL)
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

add_synonym <- function(keyword, synonym) {
  .check_length(keyword, 1)
  .check_input(keyword, "character")
  synonym <- unlist(synonym)
  walk(
    synonym,
    ~ {
      synonym <- .x
      if (!is.character(synonym)) {
        stop(paste0(
          "Error:'synonym' must of type 'character'.\nYou provided an object of type ",
          typeof(synonym),
          "."
        ))
      }
      keyword <- str_squish(keyword)
      synonym <- str_squish(synonym)

      tibble(keyword, synonym) |>
        dbAppendTable(
          conn = gt.env$globaltrends_db,
          name = "keyword_synonyms",
          value = .,
          append = TRUE
        )

      keyword_synonyms <- collect(gt.env$tbl_synonyms)
      lst_export <- list(keyword_synonyms, keyword_synonyms)
      names(lst_export) <- list("keyword_synonyms", "keyword_synonyms")
      invisible(list2env(lst_export, envir = gt.env))

      message(paste0(
        "Successfully added synonym | keyword: ",
        keyword,
        " | synonym: ",
        synonym,
        "."
      ))
    }
  )
}
