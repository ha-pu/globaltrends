#' @title Download data for object keyword batches
#'
#' @aliases download_object download_object.numeric download_object.list
#'
#' @description
#' Downloads Google Trends search volumes for one or more *object* batches
#' across a set of locations and appends the results to the database table
#' `data_object`. Each object batch is downloaded together with one control
#' keyword so that object hits can be mapped to the control scale used
#' elsewhere in the package.
#'
#' @details
#' **Prerequisites.** [start_db()] must be called before `download_object()`.
#' It connects to the database and populates `gt.env$keywords_object` and
#' `gt.env$time_object` from the tables `batch_keywords` and `batch_time`
#' (created via [add_keyword()]). These in-memory objects are used to look up
#' the keywords and time window for each requested batch. `data_control` for
#' the chosen control batch must also be present, as it is used to select an
#' appropriate control keyword per location.
#'
#' **Dispatch.** `download_object()` is an S3 generic that dispatches on the
#' class of `object`. Passing a numeric scalar routes to the `.numeric` method,
#' which performs the actual download. Passing a numeric vector of length > 1
#' coerces `object` to a list and delegates to the `.list` method, which
#' iterates over batches sequentially. Passing a list directly also routes to
#' the `.list` method.
#'
#' **Control keyword selection.** For each location the function queries
#' `data_control` for the chosen control batch, ranks control keywords by their
#' average `hits` in ascending order, and tries them one by one until one
#' yields non-zero signal in the returned series. Trying lower-signal keywords
#' first reduces saturation risk. If no control keyword produces usable signal,
#' the function stops with an informative error.
#'
#' **Download backend.** Requests are issued through the internal `.get_trend()`
#' helper, which uses either `gtrendsR::gtrends()` (default) or the Google
#' Trends Research API when [initialize_python()] has been called.
#'
#' **Deduplication.** Before downloading, the function queries `data_object`
#' for locations already present for the requested `(batch_c, batch_o)` pair.
#' Only locations not yet in the database are downloaded. If all locations are
#' already present, the function returns early with a message and no requests
#' are made.
#'
#' **Missing control baseline.** If `data_control` contains no rows for a
#' given location, that location is skipped with a message (nothing is written
#' to `data_object`).
#'
#' @section Category codes:
#' Avoid category codes unless you are confident they apply uniformly to all
#' keywords in the batch. Google Trends applies a category constraint to the
#' entire request, which can unintentionally change the meaning of control and
#' object keywords.
#'
#' @param object Numeric scalar, numeric vector, or list of numeric scalars.
#'   The object batch id(s) (`batch_o`) to download. A scalar downloads a
#'   single batch; a vector or list downloads multiple batches sequentially.
#'   Must refer to batches already registered via [add_keyword()].
#'
#' @param control Numeric scalar. Control batch id (`batch_c`) used to map
#'   object hits onto the control scale. Must refer to a batch already
#'   downloaded via [download_control()]. Defaults to `1`.
#'
#' @param locations Character vector of ISO 3166-1 alpha-2 location codes.
#'   Defaults to `gt.env$countries` when set by [start_db()]; otherwise falls
#'   back to `globaltrends::countries`. Pass `"world"` (or use
#'   [download_object_global()]) to download the worldwide aggregate instead of
#'   country-level data.
#'
#' @return
#' Invisibly returns `TRUE`. The function is called for its side effects:
#' downloaded rows are appended to `data_object` in the active database, and
#' one progress message is emitted per location. Locations with no control
#' baseline in `data_control` are skipped with a message.
#'
#' @seealso
#' [start_db()] to connect to the database and populate `gt.env`.
#' [add_keyword()] to register object batches before downloading.
#' [download_object_global()] for a convenience wrapper for worldwide data.
#' [download_control()] to download control keyword data used for scaling.
#'
#' @examples
#' \dontrun{
#' # Download one object batch for all countries
#' download_object(object = 1, control = 1, locations = countries)
#'
#' # Download several batches sequentially
#' download_object(object = as.list(1:5), control = 1, locations = countries)
#'
#' # Download worldwide aggregate
#' download_object_global(object = 1, control = 1)
#' }
#'
#' @export
#' @rdname download_object

download_object <- function(object, control = 1, locations = NULL) {
  UseMethod("download_object", object)
}

#' @rdname download_object
#' @method download_object numeric
#' @export

download_object.numeric <- function(object, control = 1, locations = NULL) {
  control <- unlist(control)
  .check_length(control, 1)
  .check_batch(control)

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
    download_object(
      object = as.list(object),
      control = control,
      locations = locations
    )
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

  # Avoid duplicate downloads: only fetch locations not yet present for this batch.
  existing <- .get_full(
    table = "data_object",
    in_batch_c = control,
    in_batch_o = object
  )
  loc_remaining <- locations[!(locations %in% existing)]

  if (length(loc_remaining) == 0) {
    message(paste0(
      "No new locations to download | object: ",
      object,
      " | control: ",
      control,
      "."
    ))
    return(invisible(TRUE))
  }

  n_locs <- length(loc_remaining)

  for (i in seq_along(loc_remaining)) {
    loc <- loc_remaining[i]

    # We require `data_control` for the same control batch and location to
    # pick an appropriate control keyword for mapping.
    dt_c <- gt.env$dt_control
    qry_control <- as.data.frame(
      dt_c[dt_c$batch == control & dt_c$location == loc, c("keyword", "hits")]
    )

    if (nrow(qry_control) == 0) {
      message(paste0(
        "Skipped object download (missing control baseline) | object: ",
        object,
        " | control: ",
        control,
        " | location: ",
        loc,
        "."
      ))
      next
    }

    # Rank control keywords by average hits (ascending) and keep only those with signal.
    # We try control keywords with lower average hits first to reduce saturation risk.
    avg_hits <- tapply(
      qry_control$hits,
      qry_control$keyword,
      mean,
      na.rm = TRUE
    )
    avg_hits <- avg_hits[avg_hits > 0]

    if (length(avg_hits) == 0) {
      stop(
        paste0(
          "Too little signal in control batch ",
          control,
          " for location ",
          loc,
          ". ",
          "Reconsider choice of control keywords."
        ),
        call. = FALSE
      )
    }

    terms_con <- names(sort(avg_hits))

    # Try control keywords until one returns non-zero signal in the result.
    success <- FALSE
    out <- NULL

    for (term_c in terms_con) {
      if (identical(loc, "world")) {
        if (isTRUE(gt.env$py_setup)) {
          out <- .get_trend(
            term = c(term_c, terms_obj),
            start_date = start_date,
            end_date = end_date
          )
        } else {
          out <- .get_trend(
            term = c(term_c, terms_obj),
            start_date = start_date,
            end_date = end_date,
            location = ""
          )
        }
      } else {
        out <- .get_trend(
          location = loc,
          term = c(term_c, terms_obj),
          start_date = start_date,
          end_date = end_date
        )
      }

      # Accept only if we got data and the control term has positive mean hits.
      if (!is.null(out)) {
        ctrl_hits <- out$hits[out$keyword == term_c]
        if (length(ctrl_hits) > 0 && mean(ctrl_hits, na.rm = TRUE) > 0) {
          success <- TRUE
          break
        }
      }
    }

    if (!success) {
      stop(
        paste0(
          "Download failed: no control keyword produced usable signal for object batch ",
          object,
          " (control batch ",
          control,
          ", location ",
          loc,
          "). ",
          "Reconsider control keywords or time window."
        ),
        call. = FALSE
      )
    }

    # Persist data
    out$batch_c <- control
    out$batch_o <- object
    gt.env$dt_object <- data.table::rbindlist(
      list(gt.env$dt_object, data.table::setDT(out)),
      use.names = TRUE
    )

    message(paste0(
      "Downloaded object data | object: ",
      object,
      " | control: ",
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

  # `rbindlist()` above drops the key set in `start_db()`. Re-key once per
  # batch (not per location) so the next `.get_full()` call can binary
  # search instead of scanning the full table.
  data.table::setkey(gt.env$dt_object, batch_c, batch_o, location)

  invisible(TRUE)
}

#' @rdname download_object
#' @method download_object list
#' @export

download_object.list <- function(object, control = 1, locations = NULL) {
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")

  for (o in object) {
    download_object(o, control = control, locations = locations)
  }
  invisible(TRUE)
}

#' @title Download worldwide aggregate object data
#'
#' @description
#' Convenience wrapper around [download_object()] that downloads the worldwide
#' aggregate instead of country-level data. Equivalent to calling
#' `download_object(object, control, locations = "world")`.
#'
#' @param object Numeric scalar, numeric vector, or list of numeric scalars.
#'   Object batch id(s) to download.
#' @param control Numeric scalar. Control batch id used for mapping. Defaults to `1`.
#'
#' @return Invisibly returns `TRUE`. See [download_object()] for details on
#'   side effects and emitted messages.
#'
#' @export
#' @rdname download_object

download_object_global <- function(object, control = 1) {
  download_object(object = object, control = control, locations = "world")
}
