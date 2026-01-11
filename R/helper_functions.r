#' @title Download Google Trends time series for one request
#'
#' @description
#' Internal helper that downloads an interest-over-time series for one or more
#' keywords for a single location and time window. Depending on configuration,
#' it uses either:
#' - the Research API backend (Python) initialized via `initialize_python()`, or
#' - `gtrendsR::gtrends()` (unofficial scraping endpoint).
#'
#' The function returns a standardized tibble with columns:
#' `location`, `keyword`, `date`, `hits`.
#'
#' @param location Character scalar. Location code accepted by Google Trends.
#'   Use `""` or `NULL` for the global aggregate. Default is `NULL`.
#' @param term Character vector. Keyword(s) to request.
#' @param start_date,end_date Character scalars in `"YYYY-MM"` format defining
#'   the start and end month of the requested time range.
#'
#' @return A tibble with columns `location`, `keyword`, `date`, `hits`, or `NULL`
#'   if no interest-over-time data is returned.
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr mutate select
#' @importFrom lubridate as_date
#' @importFrom purrr map_chr map_dbl map_dfr
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom tibble tibble

.get_trend <- function(
  location = NULL,
  term,
  start_date = "2020-01",
  end_date = "2020-12"
) {
  .check_input(term, "character")
  .check_length(start_date, 1)
  .check_length(end_date, 1)
  .check_input(start_date, "character")
  .check_input(end_date, "character")

  # Normalize location semantics: NULL/""
  is_global <- is.null(location) || identical(location, "")
  geo <- location

  if (isTRUE(gt.env$py_setup)) {
    # ---------------------------------------------------------------------
    # Research API backend (Python via reticulate)
    # ---------------------------------------------------------------------
    out <- gt.env$query_gtrends(
      terms = term,
      start_date = start_date,
      end_date = end_date,
      geo = geo,
      api_key = gt.env$api_key
    )

    # `out$lines` is expected to be list-like; each element has $term and $points.
    ts <- map_dfr(
      out$lines,
      ~ {
        kw <- .x$term
        values <- map_dbl(.x$points, ~ .x$value)
        dates <- map_chr(.x$points, ~ .x$date)
        tibble(keyword = kw, date = as.Date(dates), hits = values)
      }
    )

    ts$location <- if (is_global) "world" else location

    # Respect configured wait between API calls
    Sys.sleep(gt.env$query_wait)
    return(ts)
  }

  # -----------------------------------------------------------------------
  # gtrendsR backend
  # -----------------------------------------------------------------------
  geo <- if (is_global) "" else location
  time <- paste0(start_date, "-01 ", end_date, "-01")

  out <- .retry_gtrends(
    keyword = term,
    geo = geo,
    time = time,
    onlyInterest = TRUE
  )
  if (is.null(out) || is.null(out$interest_over_time)) {
    return(NULL)
  }

  ts <- out$interest_over_time |>
    mutate(
      hits = as.double(str_replace(.data$hits, "<1", "0.1")),
      date = as_date(.data$date)
    ) |>
    select(location = .data$geo, .data$keyword, .data$date, .data$hits)

  # Add some jitter to reduce rate-limit risk.
  Sys.sleep(stats::runif(1, min = 5, max = 10))
  ts
}

#' @title Retry wrapper for gtrendsR downloads
#'
#' @description
#' Internal wrapper around `gtrendsR::gtrends()` that retries transient failures.
#' The retry/backoff strategy matches the intent of the original implementation:
#' - retry quickly for HTTP 500 responses,
#' - otherwise wait longer before retrying.
#'
#' @param ... Passed to `gtrendsR::gtrends()`.
#'
#' @return The object returned by `gtrendsR::gtrends()`, or `NULL` if no usable
#'   result can be obtained (after exhausting retries).
#'
#' @keywords internal
#' @noRd
#' @importFrom gtrendsR gtrends
#' @importFrom stringr str_detect

.retry_gtrends <- function(..., max_tries = 10) {
  i <- 1L
  out <- try(gtrends(...), silent = TRUE)

  while (inherits(out, "try-error") && i < max_tries) {
    msg <- conditionMessage(attr(out, "condition"))
    is_500 <- isTRUE(str_detect(msg, "Returned status code:500"))

    if (is_500) {
      message("globaltrends retrying download in 1s (HTTP 500).")
      Sys.sleep(1)
    } else {
      message("globaltrends retrying download in 60s.")
      Sys.sleep(60)
    }

    i <- i + 1L
    out <- try(gtrends(...), silent = TRUE)
  }

  if (inherits(out, "try-error")) {
    stop(
      paste0(
        "Download failed after ",
        max_tries,
        " attempts.\n",
        "Last error: ",
        conditionMessage(attr(out, "condition"))
      ),
      call. = FALSE
    )
  }

  out
}

#' @title Test whether DOI data exists for given identifiers
#'
#' @description
#' Checks whether `data_doi` already contains entries for a given combination
#' of `(batch_c, batch_o, locations)`. This is used to avoid duplicating DOI
#' computation and writes.
#'
#' @param batch_c Integer-like scalar. Control batch id.
#' @param batch_o Integer-like scalar. Object batch id.
#' @param locations Character scalar. Location set name (e.g., `"countries"`).
#'
#' @return Logical scalar. `TRUE` if no matching rows exist; `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect filter
#' @importFrom rlang .data

.test_empty <- function(batch_c = NULL, batch_o = NULL, locations = NULL) {
  .check_batch(batch_c)
  .check_batch(batch_o)
  if (!is.null(locations)) {
    .check_locations(locations)
  }

  out <- gt.env$tbl_doi |>
    filter(
      .data$batch_c == batch_c,
      .data$batch_o == batch_o,
      .data$locations == locations
    ) |>
    utils::head(1) |>
    collect()

  nrow(out) == 0
}

#' @title List locations already present for a batch combination
#'
#' @description
#' Returns the set of location codes that already exist in a data table for a
#' given batch combination. Used to avoid duplicate downloads/computations.
#'
#' Supported `table` values:
#' - `"data_control"`: filters on `data_control.batch == batch_c`
#' - `"data_object"`: filters on `data_object.batch_c == batch_c` and `batch_o == batch_o`
#' - `"data_score"`:  filters on `data_score.batch_c == batch_c` and `batch_o == batch_o`
#'
#' @param table Character scalar. One of `"data_control"`, `"data_object"`,
#'   `"data_score"`.
#' @param batch_c Integer-like scalar. Control batch id.
#' @param batch_o Integer-like scalar. Object batch id (required for object/score).
#'
#' @return Character vector of distinct location codes present in the table for
#'   the specified identifiers.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect count filter pull select
#' @importFrom rlang .data

.get_full <- function(table, in_batch_c = NULL, in_batch_o = NULL) {
  .check_input(table, "character")
  .check_length(table, 1)

  if (!is.null(in_batch_c)) {
    .check_batch(in_batch_c)
  }
  if (!is.null(in_batch_o)) {
    .check_batch(in_batch_o)
  }

  tbl <- switch(
    table,
    data_control = gt.env$tbl_control |>
      filter(.data$batch == in_batch_c),
    data_object = {
      if (is.null(in_batch_o)) {
        stop(
          "`batch_o` must be provided for table = 'data_object'.",
          call. = FALSE
        )
      }
      gt.env$tbl_object |>
        filter(.data$batch_c == in_batch_c, .data$batch_o == in_batch_o)
    },
    data_score = {
      if (is.null(in_batch_o)) {
        stop(
          "`batch_o` must be provided for table = 'data_score'.",
          call. = FALSE
        )
      }
      gt.env$tbl_score |>
        filter(.data$batch_c == in_batch_c, .data$batch_o == in_batch_o)
    },
    stop(
      "Error: `table` must be one of 'data_control', 'data_object', or 'data_score'.",
      call. = FALSE
    )
  )

  tbl |>
    count(.data$location, name = "n") |>
    select(.data$location) |>
    collect() |>
    pull(.data$location)
}
