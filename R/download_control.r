#' @title Download data for control keyword batches
#'
#' @aliases download_control download_control.numeric download_control.list
#'
#' @description
#' Downloads Google Trends search volumes for one or more *control* batches
#' across a set of locations and appends the results to the database table
#' `data_control`.
#'
#' @details
#' **Prerequisites.** [start_db()] must be called before `download_control()`.
#' It connects to the database and populates `gt.env$keywords_control` and
#' `gt.env$time_control` from the tables `batch_keywords` and `batch_time`
#' (created via [add_keyword()]). These in-memory objects are used to look up
#' the keywords and time window for each requested batch.
#'
#' **Dispatch.** `download_control()` is an S3 generic that dispatches on the
#' class of `control`. Passing a numeric scalar routes to the `.numeric` method,
#' which performs the actual download. Passing a numeric vector of length > 1
#' coerces `control` to a list and delegates to the `.list` method, which
#' iterates over batches sequentially. Passing a list directly also routes to
#' the `.list` method.
#'
#' **Download backend.** Requests are issued through the internal `.get_trend()`
#' helper, which uses either `gtrendsR::gtrends()` (default) or the Google
#' Trends Research API when [initialize_python()] has been called.
#'
#' **Deduplication.** Before downloading, the function queries `data_control`
#' for locations already present for the requested batch. Only locations not yet
#' in the database are downloaded. If all locations are already present, the
#' function returns early with a message and no requests are made.
#'
#' **Missing data.** If the API returns no data for a location (e.g. due to
#' insufficient search volume), the result for that location is silently skipped
#' (nothing is written to `data_control`) and a "No data returned" message is
#' emitted.
#'
#' @section Category codes:
#' Avoid category codes unless you are confident they apply uniformly to all
#' keywords in the batch. Google Trends applies a category constraint to the
#' entire request, which can unintentionally change the meaning of control and
#' object keywords.
#'
#' @param control Numeric scalar, numeric vector, or list of numeric scalars.
#'   The control batch id(s) to download. A scalar downloads a single batch; a
#'   vector or list downloads multiple batches sequentially. Must refer to
#'   batches already registered via [add_keyword()].
#'
#' @param locations Character vector of ISO 3166-1 alpha-2 location codes.
#'   Defaults to `gt.env$countries` when set by [start_db()]; otherwise falls
#'   back to `globaltrends::countries`. Pass `"world"` (or use
#'   [download_control_global()]) to download the worldwide aggregate instead of
#'   country-level data.
#'
#' @return
#' Invisibly returns `TRUE`. The function is called for its side effects:
#' downloaded rows are appended to `data_control` in the active database, and
#' one progress message is emitted per location indicating whether data was
#' written or no data was returned.
#'
#' @seealso
#' [start_db()] to connect to the database and populate `gt.env`.
#' [add_keyword()] to register control batches before downloading.
#' [download_control_global()] for a convenience wrapper for worldwide data.
#' [download_object()] to download object keyword data using a control batch for
#' scaling.
#'
#' @examples
#' \dontrun{
#' # Download one control batch for all countries
#' download_control(control = 1, locations = countries)
#'
#' # Download several batches sequentially
#' download_control(control = as.list(1:5), locations = countries)
#'
#' # Download worldwide aggregate
#' download_control_global(control = 1)
#' }
#'
#' @export
#' @rdname download_control
#' @importFrom DBI dbAppendTable

download_control <- function(control, locations = NULL) {
  UseMethod("download_control", control)
}

#' @rdname download_control
#' @method download_control numeric
#' @export

download_control.numeric <- function(control, locations = NULL) {
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")

  # Vector input: delegate to list method for consistent iteration semantics.
  if (length(control) > 1) {
    download_control(control = as.list(control), locations = locations)
    return(invisible(TRUE))
  }

  .check_batch(control)

  # Validate that the batch metadata is present (start_db() should have run).
  if (is.null(gt.env$keywords_control) || is.null(gt.env$time_control)) {
    stop(
      "Control batch metadata not found in `gt.env`. Run `start_db()` first.",
      call. = FALSE
    )
  }

  terms <- gt.env$keywords_control$keyword[
    gt.env$keywords_control$batch == control
  ]
  start_date <- gt.env$time_control$start_date[
    gt.env$time_control$batch == control
  ]
  end_date <- gt.env$time_control$end_date[gt.env$time_control$batch == control]

  if (length(terms) == 0) {
    stop(
      paste0("No keywords found for control batch ", control, "."),
      call. = FALSE
    )
  }
  if (length(start_date) == 0 || length(end_date) == 0) {
    stop(
      paste0("No time window found for control batch ", control, "."),
      call. = FALSE
    )
  }

  # Avoid duplicate downloads: only fetch locations not yet present for this batch.
  existing <- .get_full(table = "data_control", in_batch_c = control)
  loc_remaining <- locations[!(locations %in% existing)]

  if (length(loc_remaining) == 0) {
    message(paste0("No new locations to download | control: ", control, "."))
    return(invisible(TRUE))
  }

  n_locs <- length(loc_remaining)
  for (i in seq_along(loc_remaining)) {
    loc <- loc_remaining[i]

    # Global download: gtrendsR uses geo = "", Research API requires geo = NULL
    # (Python's _geo_kwargs() only omits restrictions_geo when geo is None).
    out <- if (identical(loc, "world")) {
      if (isTRUE(gt.env$py_setup)) {
        .get_trend(term = terms, start_date = start_date, end_date = end_date)
      } else {
        .get_trend(
          term = terms,
          start_date = start_date,
          end_date = end_date,
          location = ""
        )
      }
    } else {
      .get_trend(
        location = loc,
        term = terms,
        start_date = start_date,
        end_date = end_date
      )
    }

    if (!is.null(out)) {
      out$batch <- control
      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "data_control",
        value = out
      )
      message(paste0(
        "Downloaded control data | control: ",
        control,
        " | location: ",
        loc,
        " [",
        i,
        "/",
        n_locs,
        "]"
      ))
    } else {
      message(paste0(
        "No data returned | control: ",
        control,
        " | location: ",
        loc,
        " [",
        i,
        "/",
        n_locs,
        "]"
      ))
    }
  }

  invisible(TRUE)
}

#' @rdname download_control
#' @method download_control list
#' @export

download_control.list <- function(control, locations = NULL) {
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")

  for (c in control) {
    download_control(c, locations = locations)
  }
  invisible(TRUE)
}

#' @title Download worldwide aggregate control data
#'
#' @description
#' Convenience wrapper around [download_control()] that downloads the worldwide
#' aggregate instead of country-level data. Equivalent to calling
#' `download_control(control, locations = "world")`.
#'
#' @param control Numeric scalar, numeric vector, or list of numeric scalars.
#'   Control batch id(s) to download.
#'
#' @return Invisibly returns `TRUE`. See [download_control()] for details on
#'   side effects and emitted messages.
#'
#' @export
#' @rdname download_control

download_control_global <- function(control) {
  download_control(control = control, locations = "world")
}
