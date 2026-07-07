#' @keywords internal
#' @noRd
#'
#' Single seam for all rate-limit/backoff pauses. Base functions cannot be
#' mocked with testthat::local_mocked_bindings(), so tests stub this wrapper
#' instead of Sys.sleep() to run retry logic without real waits.

.wait <- function(seconds) {
  Sys.sleep(seconds)
  invisible(NULL)
}

#' @keywords internal
#' @noRd

.increment_api_counter <- function() {
  today <- Sys.Date()
  if (!identical(gt.env$api_calls_date, today)) {
    gt.env$api_calls <- 0L
    gt.env$api_calls_date <- today
  }
  gt.env$api_calls <- gt.env$api_calls + 1L

  if (gt.env$api_calls %% 1000L == 0L && !is.null(gt.env$dt_control)) {
    message(
      "Persisting in-memory data to local file after ",
      gt.env$api_calls,
      " API calls."
    )
    disconnect_db()
    start_db()
  }
}

#' @keywords internal
#' @noRd

.increment_score_counter <- function() {
  if (is.null(gt.env$score_calls)) {
    gt.env$score_calls <- 0L
  }
  gt.env$score_calls <- gt.env$score_calls + 1L

  if (gt.env$score_calls %% 1000L == 0L && !is.null(gt.env$dt_control)) {
    message(
      "Persisting in-memory data to local file after ",
      gt.env$score_calls,
      " computed locations."
    )
    disconnect_db()
    start_db()
  }
}

#' @title Download Google Trends time series for one request
#'
#' @description
#' Internal helper that downloads an interest-over-time series for one or more
#' keywords for a single location and time window. Depending on configuration,
#' it uses either:
#' - the Research API backend (Python) initialized via `initialize_python()`, or
#' - `gtrendsR::gtrends()` (unofficial scraping endpoint).
#'
#' The function returns a data frame with columns:
#' `location`, `keyword`, `date`, `hits`.
#'
#' @param location Character scalar. Location code accepted by Google Trends.
#'   Use `""` or `NULL` for the global aggregate. Default is `NULL`.
#' @param term Character vector. Keyword(s) to request.
#' @param start_date,end_date Character scalars in `"YYYY-MM"` format defining
#'   the start and end month of the requested time range.
#'
#' @return A data frame with columns `location`, `keyword`, `date`, `hits`, or `NULL`
#'   if no interest-over-time data is returned.
#'
#' @keywords internal
#' @noRd

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
    out <- tryCatch(
      .retry_py_call(function() {
        gt.env$query_trend(
          terms = term,
          start_date = start_date,
          end_date = end_date,
          geo = geo,
          api_key = gt.env$api_key
        )
      }),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("429|rateLimitExceeded|Quota exceeded", msg)) {
          tryCatch(disconnect_db(), error = function(dc_err) {
            message("disconnect_db() failed: ", conditionMessage(dc_err))
          })
          stop(
            paste0(
              "Google Trends API daily quota exceeded. ",
              "Wait until quota resets before continuing.\n",
              "Original error: ",
              msg
            ),
            call. = FALSE
          )
        }
        if (grepl("400|badRequest|invalid argument", msg, ignore.case = TRUE)) {
          message(
            "Skipping: API returned HTTP 400 (invalid argument) for term=",
            paste(term, collapse = ","),
            " geo=",
            if (is.null(geo)) "world" else geo,
            " [",
            start_date,
            "/",
            end_date,
            "]"
          )
          return(NULL)
        }
        if (
          grepl(
            "TimeoutError|WinError 10060|timed out",
            msg,
            ignore.case = TRUE
          )
        ) {
          message(
            "Skipping: connection timeout for term=",
            paste(term, collapse = ","),
            " geo=",
            if (is.null(geo)) "world" else geo,
            " [",
            start_date,
            "/",
            end_date,
            "]"
          )
          return(NULL)
        }
        stop(e)
      }
    )

    if (is.null(out)) {
      return(NULL)
    }

    ts <- do.call(
      rbind,
      lapply(out$lines, function(line) {
        data.frame(
          keyword = line$term,
          date = as.Date(vapply(line$points, function(p) p$date, character(1))),
          hits = vapply(line$points, function(p) p$value, numeric(1)),
          stringsAsFactors = FALSE
        )
      })
    )

    ts$location <- if (is_global) "world" else location

    .increment_api_counter()
    # Respect configured wait between API calls
    .wait(gt.env$query_wait)
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

  ts_raw <- out$interest_over_time
  ts_raw$hits <- as.double(sub("<1", "0.1", ts_raw$hits, fixed = TRUE))
  ts_raw$date <- as.Date(ts_raw$date)
  ts <- ts_raw[, c("geo", "keyword", "date", "hits")]
  names(ts)[1] <- "location"

  # Add some jitter to reduce rate-limit risk.
  .wait(stats::runif(1, min = 5, max = 10))
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

.retry_gtrends <- function(..., max_tries = 10) {
  i <- 1L
  out <- try(gtrends(...), silent = TRUE)

  while (inherits(out, "try-error") && i < max_tries) {
    msg <- conditionMessage(attr(out, "condition"))
    is_500 <- isTRUE(grepl("Returned status code:500", msg, fixed = TRUE))

    if (is_500) {
      message("globaltrends retrying download in 1s (HTTP 500).")
      .wait(1)
    } else {
      message("globaltrends retrying download in 60s.")
      .wait(60)
    }

    i <- i + 1L
    out <- try(gtrends(...), silent = TRUE)
  }

  if (inherits(out, "try-error")) {
    stop(
      sprintf(
        "Download failed after %d attempts.\nLast error: %s",
        max_tries,
        conditionMessage(attr(out, "condition"))
      ),
      call. = FALSE
    )
  }

  out
}

#' @title Retry wrapper for Research API (Python) calls
#'
#' @description
#' Internal wrapper around calls to `gt.env$query_trend()`,
#' `gt.env$query_region()`, and `gt.env$query_terms()` that retries transient
#' server-side failures (e.g. HTTP 502/503/504) with exponential backoff.
#' Non-transient errors (e.g. 429 quota, 400 bad request, timeouts) are
#' rethrown immediately so the caller's own `tryCatch()` handler can deal with
#' them.
#'
#' @param call A zero-argument function performing the API call.
#' @param max_tries Integer scalar. Maximum number of attempts. Defaults to `5`.
#' @param wait Numeric scalar. Seconds to wait before the first retry; doubles
#'   after each subsequent transient failure. Defaults to `5`.
#'
#' @return The value returned by `call()`.
#'
#' @keywords internal
#' @noRd

.retry_py_call <- function(call, max_tries = 5, wait = 5) {
  attempt <- 1L
  repeat {
    result <- tryCatch(
      list(value = call()),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      return(result$value)
    }

    msg <- conditionMessage(result)
    is_transient <- grepl(
      "502|503|504|Bad Gateway|Service Unavailable|Gateway Timeout|backendError|internalError",
      msg,
      ignore.case = TRUE
    )

    if (!is_transient || attempt >= max_tries) {
      stop(result)
    }

    message(
      "Transient Google Trends API error (attempt ",
      attempt,
      "/",
      max_tries,
      "). Retrying in ",
      wait,
      "s.\n",
      msg
    )
    .wait(wait)
    attempt <- attempt + 1L
    wait <- wait * 2
  }
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

.test_empty <- function(batch_c = NULL, batch_o = NULL, locations = NULL) {
  .check_batch(batch_c)
  .check_batch(batch_o)
  if (!is.null(locations)) {
    .check_locations(locations)
  }

  # `target_*` avoid colliding with `dt_doi`'s `batch_c`/`batch_o`/`locations`
  # columns: under data.table's NSE (see `.datatable.aware` in zzz.r), a bare
  # symbol that matches a column name resolves to the COLUMN, not this
  # argument, which would make the filter always-true.
  target_batch_c <- batch_c
  target_batch_o <- batch_o
  target_locations <- locations

  dt <- gt.env$dt_doi
  n <- nrow(dt[
    dt$batch_c == target_batch_c &
      dt$batch_o == target_batch_o &
      dt$locations == target_locations,
  ])

  n == 0L
}

#' @title List locations already present for a batch combination
#'
#' @description
#' Returns the set of location codes that already exist in a data table for a
#' given batch combination. Used to avoid duplicate downloads/computations.
#'
#' Supported `table` values and their filter logic:
#' - `"data_control"`: `batch == in_batch_c`
#' - `"data_object"`: `batch_c == in_batch_c` and `batch_o == in_batch_o`
#' - `"data_score"`:  `batch_c == in_batch_c` and `batch_o == in_batch_o`
#' - `"data_region"`: `batch_o == in_batch_o`
#' - `"data_related"`: `batch_o == in_batch_o`, `topic == in_topic`, `rising == in_rising`
#'
#' @param table Character scalar. One of `"data_control"`, `"data_object"`,
#'   `"data_score"`, `"data_region"`, or `"data_related"`.
#' @param in_batch_c Integer-like scalar. Control batch id. Required for
#'   `"data_control"`, `"data_object"`, and `"data_score"`.
#' @param in_batch_o Integer-like scalar. Object batch id. Required for all
#'   tables except `"data_control"`.
#' @param in_topic Logical scalar. Whether to filter for topics (`TRUE`) or
#'   queries (`FALSE`). Required for `"data_related"`.
#' @param in_rising Logical scalar. Whether to filter for rising (`TRUE`) or
#'   top (`FALSE`) terms. Required for `"data_related"`.
#'
#' @return Character vector of distinct location codes present in the table for
#'   the specified identifiers. Returns `character(0)` if no matching rows exist.
#'
#' @keywords internal
#' @noRd

.get_full <- function(
  table,
  in_batch_c = NULL,
  in_batch_o = NULL,
  in_topic = NULL,
  in_rising = NULL
) {
  .check_input(table, "character")
  .check_length(table, 1)

  if (!is.null(in_batch_c)) {
    .check_batch(in_batch_c)
  }
  if (!is.null(in_batch_o)) {
    .check_batch(in_batch_o)
  }
  if (!is.null(in_topic)) {
    .check_input(in_topic, "logical")
  }
  if (!is.null(in_rising)) {
    .check_input(in_rising, "logical")
  }

  # Each branch below uses `dt[.(values), location, on = "cols", nomatch =
  # NULL]` rather than `dt[dt$col == value, ]$location`. With this namespace
  # declared data.table-aware (see `.datatable.aware` in zzz.r), `on =`
  # drives a binary-search join instead of a full vector scan over every
  # row, and - unlike relying on `dt` carrying a persistent `key()` - works
  # regardless of key state. This matters because `rbindlist()` (used by
  # every download/compute function to append new rows) drops keys, so `dt`
  # would otherwise be unkeyed again well before the next `.get_full()`
  # call; download/compute functions re-key their table after each batch's
  # appends (see e.g. `download_control.r`) so the join here stays fast.
  switch(
    table,
    data_control = {
      if (is.null(in_batch_c)) {
        stop(
          "`batch_c` must be provided for table = 'data_control'.",
          call. = FALSE
        )
      }
      dt <- gt.env$dt_control
      if (nrow(dt) == 0L) {
        character(0)
      } else {
        unique(dt[
          list(as.integer(in_batch_c)),
          location,
          on = "batch",
          nomatch = NULL
        ])
      }
    },
    data_object = {
      if (is.null(in_batch_o)) {
        stop(
          "`batch_o` must be provided for table = 'data_object'.",
          call. = FALSE
        )
      }
      dt <- gt.env$dt_object
      if (nrow(dt) == 0L) {
        character(0)
      } else {
        unique(dt[
          list(as.integer(in_batch_c), as.integer(in_batch_o)),
          location,
          on = c("batch_c", "batch_o"),
          nomatch = NULL
        ])
      }
    },
    data_score = {
      if (is.null(in_batch_o)) {
        stop(
          "`batch_o` must be provided for table = 'data_score'.",
          call. = FALSE
        )
      }
      dt <- gt.env$dt_score
      if (nrow(dt) == 0L) {
        character(0)
      } else {
        unique(dt[
          list(as.integer(in_batch_c), as.integer(in_batch_o)),
          location,
          on = c("batch_c", "batch_o"),
          nomatch = NULL
        ])
      }
    },
    data_region = {
      if (is.null(in_batch_o)) {
        stop(
          "`batch_o` must be provided for table = 'data_region'.",
          call. = FALSE
        )
      }
      dt <- gt.env$dt_region
      if (nrow(dt) == 0L) {
        character(0)
      } else {
        unique(dt[
          list(as.integer(in_batch_o)),
          location,
          on = "batch_o",
          nomatch = NULL
        ])
      }
    },
    data_related = {
      if (is.null(in_batch_o)) {
        stop(
          "`batch_o` must be provided for table = 'data_related'.",
          call. = FALSE
        )
      }
      if (is.null(in_topic)) {
        stop(
          "`topic` must be provided for table = 'data_related'.",
          call. = FALSE
        )
      }
      if (is.null(in_rising)) {
        stop(
          "`rising` must be provided for table = 'data_related'.",
          call. = FALSE
        )
      }
      dt <- gt.env$dt_related
      if (nrow(dt) == 0L) {
        character(0)
      } else {
        unique(dt[
          list(
            as.integer(in_batch_o),
            as.integer(in_topic),
            as.integer(in_rising)
          ),
          location,
          on = c("batch_o", "topic", "rising"),
          nomatch = NULL
        ])
      }
    },
    stop(
      "Error: `table` must be one of 'data_control', 'data_object', 'data_score', 'data_region', or 'data_related'.",
      call. = FALSE
    )
  )
}

#' @title Download Google Trends regional interest breakdown for one request
#'
#' @description
#' Internal helper that downloads a sub-regional interest breakdown for a single
#' keyword, location, and time window using the Research API (Python) backend.
#' There is no `gtrendsR` fallback; the function requires `initialize_python()`
#' to have been called first.
#'
#' @param location Character scalar. Location code accepted by Google Trends.
#'   Use `""` or `NULL` for the global aggregate. Default is `NULL`.
#' @param term Character scalar. Single keyword to request.
#' @param start_date Character scalar in `"YYYY-MM"` format defining the start
#'   month of the requested time range.
#' @param end_date Character scalar in `"YYYY-MM"` format defining the end
#'   month of the requested time range.
#'
#' @return A data frame with columns `term`, `location`, `start_date`, `end_date`,
#'   `region_code`, `region_name`, and `hits`. If the Python query fails, returns
#'   a one-row data frame with all columns set to `NA`.
#'
#' @keywords internal
#' @noRd

.get_region <- function(
  location = NULL,
  term,
  start_date = "2020-01",
  end_date = "2020-12"
) {
  .check_length(term, 1)
  .check_length(start_date, 1)
  .check_length(end_date, 1)
  .check_input(term, "character")
  .check_input(start_date, "character")
  .check_input(end_date, "character")

  # Normalize location semantics: NULL/""
  is_global <- is.null(location) || identical(location, "")
  geo <- location

  if (isTRUE(gt.env$py_setup)) {
    # ---------------------------------------------------------------------
    # Research API backend (Python via reticulate)
    # ---------------------------------------------------------------------
    out <- tryCatch(
      .retry_py_call(function() {
        gt.env$query_region(
          terms = term,
          start_date = start_date,
          end_date = end_date,
          geo = geo,
          api_key = gt.env$api_key
        )
      }),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("429|rateLimitExceeded|Quota exceeded", msg)) {
          tryCatch(disconnect_db(), error = function(dc_err) {
            message("disconnect_db() failed: ", conditionMessage(dc_err))
          })
          stop(
            paste0(
              "Google Trends API daily quota exceeded. ",
              "Wait until quota resets before continuing.\n",
              "Original error: ",
              msg
            ),
            call. = FALSE
          )
        }
        if (grepl("400|badRequest|invalid argument", msg, ignore.case = TRUE)) {
          message(
            "Skipping: API returned HTTP 400 (invalid argument) for term=",
            term,
            " geo=",
            if (is.null(geo)) "world" else geo,
            " [",
            start_date,
            "/",
            end_date,
            "]"
          )
        } else if (
          grepl(
            "TimeoutError|WinError 10060|timed out",
            msg,
            ignore.case = TRUE
          )
        ) {
          message(
            "Skipping: connection timeout for term=",
            term,
            " geo=",
            if (is.null(geo)) "world" else geo,
            " [",
            start_date,
            "/",
            end_date,
            "]"
          )
        } else {
          stop(e)
        }
        data.frame(
          term = NA_character_,
          location = NA_character_,
          start_date = as.Date(NA),
          end_date = as.Date(NA),
          region_code = NA_character_,
          region_name = NA_character_,
          hits = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    )

    region <- do.call(
      rbind,
      lapply(out$regions, function(r) {
        data.frame(
          region_code = r$regionCode,
          region_name = r$regionName,
          hits = r$value,
          stringsAsFactors = FALSE
        )
      })
    )
    region$term <- term
    region$location <- if (is_global) "world" else location
    region$start_date <- as.Date(paste0(start_date, "-01"))
    region$end_date <- as.Date(paste0(end_date, "-01"))
    region <- region[, c(
      "term",
      "location",
      "start_date",
      "end_date",
      "region_code",
      "region_name",
      "hits"
    )]

    .increment_api_counter()
    # Respect configured wait between API calls
    .wait(gt.env$query_wait)
    return(region)
  }
}

#' @title Download Google Trends related queries or topics for one request
#'
#' @description
#' Internal helper that downloads related queries or topics from Google Trends
#' for a single keyword, location, and time window using the Research API
#' (Python) backend. There is no `gtrendsR` fallback; the function requires
#' `initialize_python()` to have been called first.
#'
#' @param location Character scalar. Location code accepted by Google Trends.
#'   Use `""` or `NULL` for the global aggregate. Default is `NULL`.
#' @param term Character scalar. Single keyword to request.
#' @param start_date Character scalar in `"YYYY-MM"` format defining the start
#'   month of the requested time range.
#' @param end_date Character scalar in `"YYYY-MM"` format defining the end
#'   month of the requested time range.
#' @param topic Logical scalar. If `TRUE`, returns related topics; if `FALSE`,
#'   returns related queries.
#' @param rising Logical scalar. If `TRUE`, returns rising (breakout) terms;
#'   if `FALSE`, returns top terms.
#'
#' @return A data frame with columns `related_term`, `hits`, `term`, `topic`,
#'   `rising`, `location`, `start_date`, and `end_date`.
#'
#' @keywords internal
#' @noRd

.get_related <- function(
  location = NULL,
  term,
  start_date = "2020-01",
  end_date = "2020-12",
  topic = NULL,
  rising = NULL
) {
  .check_length(term, 1)
  .check_length(start_date, 1)
  .check_length(end_date, 1)
  .check_input(term, "character")
  .check_input(start_date, "character")
  .check_input(end_date, "character")
  .check_input(topic, "logical")
  .check_input(rising, "logical")

  # Normalize location semantics: NULL/""
  is_global <- is.null(location) || identical(location, "")
  geo <- location

  if (isTRUE(gt.env$py_setup)) {
    # ---------------------------------------------------------------------
    # Research API backend (Python via reticulate)
    # ---------------------------------------------------------------------
    out <- tryCatch(
      .retry_py_call(function() {
        gt.env$query_terms(
          terms = term,
          start_date = start_date,
          end_date = end_date,
          geo = geo,
          api_key = gt.env$api_key,
          topic = topic,
          rising = rising
        )
      }),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("429|rateLimitExceeded|Quota exceeded", msg)) {
          tryCatch(disconnect_db(), error = function(dc_err) {
            message("disconnect_db() failed: ", conditionMessage(dc_err))
          })
          stop(
            paste0(
              "Google Trends API daily quota exceeded. ",
              "Wait until quota resets before continuing.\n",
              "Original error: ",
              msg
            ),
            call. = FALSE
          )
        }
        if (grepl("400|badRequest|invalid argument", msg, ignore.case = TRUE)) {
          message(
            "Skipping: API returned HTTP 400 (invalid argument) for term=",
            term,
            " geo=",
            if (is.null(geo)) "world" else geo,
            " [",
            start_date,
            "/",
            end_date,
            "]"
          )
          return(NULL)
        }
        if (
          grepl(
            "TimeoutError|WinError 10060|timed out",
            msg,
            ignore.case = TRUE
          )
        ) {
          message(
            "Skipping: connection timeout for term=",
            term,
            " geo=",
            if (is.null(geo)) "world" else geo,
            " [",
            start_date,
            "/",
            end_date,
            "]"
          )
          return(NULL)
        }
        stop(e)
      }
    )

    if (is.null(out)) {
      return(NULL)
    }

    item <- do.call(
      rbind,
      lapply(out$item, function(x) {
        data.frame(
          related_term = x$title,
          hits = x$value,
          stringsAsFactors = FALSE
        )
      })
    )
    item$term <- term
    item$topic <- topic
    item$rising <- rising
    item$location <- if (is_global) "world" else location
    item$start_date <- as.Date(paste0(start_date, "-01"))
    item$end_date <- as.Date(paste0(end_date, "-01"))

    .increment_api_counter()
    # Respect configured wait between API calls
    .wait(gt.env$query_wait)
    return(item)
  }
}
