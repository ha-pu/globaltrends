#' @title Download regional interest data for object keywords
#'
#' @aliases download_region download_region.numeric download_region.list download_region_global
#'
#' @description
#' Downloads regional interest data (sub-geo breakdown) for the keywords in one
#' or more object batches (`batch_o`) and writes the results to the database
#' table `data_region`.
#'
#' @details
#' **Prerequisites.** [initialize_python()] must be called before
#' `download_region()` to initialise the Research API backend. [start_db()]
#' must also have been called to connect to the database and populate
#' `gt.env$keywords_object` and `gt.env$time_object`.
#'
#' **Dispatch.** `download_region()` is an S3 generic that dispatches on the
#' class of `object`. Passing a numeric scalar routes to the `.numeric` method,
#' which performs the actual download. Passing a numeric vector of length > 1
#' coerces `object` to a list and delegates to the `.list` method, which
#' iterates over batches sequentially. Passing a list directly also routes to
#' the `.list` method.
#'
#' **Download backend.** Requests are issued through the internal `.get_region()`
#' helper using the Google Trends Research API. This backend always requires
#' Python to be set up via [initialize_python()]; unlike [download_control()],
#' no `gtrendsR` fallback is available.
#'
#' **Deduplication.** Before downloading, the function queries `data_region` for
#' locations already present for the requested object batch. Only locations not
#' yet in the database are downloaded. If all requested locations are already
#' present, the function returns early with a message and no requests are made.
#'
#' **Missing data.** If the API returns no data for a location (e.g. due to
#' insufficient search volume), the result for that location is silently skipped
#' (nothing is written to `data_region`) and a "No region data returned" message
#' is emitted.
#'
#' @param object Numeric scalar, numeric vector, or list of numeric scalars.
#'   The object batch id(s) to download. A scalar downloads a single batch; a
#'   vector or list downloads multiple batches sequentially. Must refer to
#'   batches already registered via [add_keyword()].
#'
#' @param locations Character vector of location codes. Defaults to
#'   `gt.env$countries` when set by [start_db()]; otherwise falls back to
#'   `globaltrends::countries`. Pass `"world"` (or use
#'   [download_region_global()]) to download the worldwide aggregate instead of
#'   country-level data.
#'
#' @return
#' Invisibly returns `TRUE`. The function is called for its side effects:
#' downloaded rows are appended to `data_region` in the active database, and
#' one progress message is emitted per location indicating whether data was
#' written or no data was returned.
#'
#' @seealso
#' [initialize_python()] to set up the Python backend before downloading.
#' [start_db()] to connect to the database and populate `gt.env`.
#' [add_keyword()] to register object batches before downloading.
#' [download_region_global()] for a convenience wrapper for worldwide data.
#' [download_control()] to download control keyword data.
#'
#' @examples
#' \dontrun{
#' # Download one object batch for all countries
#' initialize_python(api_key = "XXX", conda_env = "/path/to/env")
#' start_db()
#' download_region(object = 1, locations = countries)
#'
#' # Download several batches sequentially
#' download_region(object = as.list(1:3), locations = countries)
#'
#' # Download worldwide aggregate
#' download_region_global(object = 1)
#' }
#'
#' @export
#' @rdname download_region
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr filter mutate
#' @importFrom purrr iwalk map_dfr walk
#' @importFrom rlang .data

download_region <- function(object, locations = NULL) {
  UseMethod("download_region", object)
}

#' @rdname download_region
#' @method download_region numeric
#' @export

download_region.numeric <- function(object, locations = NULL) {
  if (!isTRUE(gt.env$py_setup)) {
    stop(
      "Python backend is not initialized. Run `initialize_python()` first.",
      call. = FALSE
    )
  }

  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")

  # Vector input: delegate to list method for consistent iteration semantics.
  if (length(object) > 1) {
    download_region(object = as.list(object), locations = locations)
    return(invisible(TRUE))
  }

  .check_batch(object)

  # Validate that batch metadata is present (start_db() should have run).
  if (is.null(gt.env$keywords_object) || is.null(gt.env$time_object)) {
    stop(
      "Object batch metadata not found in `gt.env`. Run `start_db()` first.",
      call. = FALSE
    )
  }

  terms_obj <- gt.env$keywords_object$keyword[
    gt.env$keywords_object$batch == object
  ]
  start_date <- gt.env$time_object$start_date[
    gt.env$time_object$batch == object
  ]
  end_date <- gt.env$time_object$end_date[gt.env$time_object$batch == object]

  if (length(terms_obj) == 0) {
    stop(
      paste0("No keywords found for object batch ", object, "."),
      call. = FALSE
    )
  }
  if (length(start_date) == 0 || length(end_date) == 0) {
    stop(
      paste0("No time window found for object batch ", object, "."),
      call. = FALSE
    )
  }

  # Avoid duplicates: only fetch locations not yet present for this batch.
  existing <- .get_full(
    table = "data_region",
    in_batch_c = NULL,
    in_batch_o = object
  )
  loc_remaining <- locations[!(locations %in% existing)]

  if (length(loc_remaining) == 0) {
    message(paste0("No new locations to download | object: ", object, "."))
    return(invisible(TRUE))
  }

  iwalk(
    loc_remaining,
    ~ {
      loc <- .x
      # location = NULL omits the geo restriction (world aggregate).
      geo <- if (identical(loc, "world")) NULL else loc

      out <- map_dfr(
        terms_obj,
        ~ .get_region(
          location = geo,
          term = .x,
          start_date = start_date,
          end_date = end_date
        )
      )
      out <- filter(out, !is.na(.data$term))

      if (nrow(out) == 0) {
        message(paste0(
          "No region data returned | object: ",
          object,
          " | location: ",
          loc,
          " [",
          .y,
          "/",
          length(loc_remaining),
          "]"
        ))
        return(invisible(TRUE))
      }

      out <- mutate(out, batch_o = object)

      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "data_region",
        value = out
      )

      message(paste0(
        "Downloaded region data | object: ",
        object,
        " | location: ",
        loc,
        " [",
        .y,
        "/",
        length(loc_remaining),
        "]"
      ))

      invisible(TRUE)
    }
  )

  invisible(TRUE)
}

#' @rdname download_region
#' @method download_region list
#' @export

download_region.list <- function(object, locations = NULL) {
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")

  walk(object, download_region, locations = locations)
  invisible(TRUE)
}

#' @title Download global regional interest data
#'
#' @description
#' Convenience wrapper around [download_region()] that downloads the worldwide
#' aggregate instead of country-level data. Equivalent to calling
#' `download_region(object, locations = "world")`.
#'
#' @param object Numeric scalar, numeric vector, or list of numeric scalars.
#'   Object batch id(s) to download.
#'
#' @return Invisibly returns `TRUE`. See [download_region()] for details on
#'   side effects and emitted messages.
#'
#' @export
#' @rdname download_region

download_region_global <- function(object) {
  download_region(object = object, locations = "world")
}
