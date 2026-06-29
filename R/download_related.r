#' @title Download related topics and themes for object keyword batches
#'
#' @aliases download_related download_related.numeric download_related.list download_topics download_themes download_topics_rising download_themes_rising download_topics_global download_themes_global download_topics_rising_global download_themes_rising_global
#'
#' @description
#' Downloads Google Trends related topics or themes for one or more *object*
#' batches across a set of locations and appends the results to the database
#' table `data_related`. Convenience wrappers cover all four combinations of
#' `topic` (topics vs. themes) and `rising` (top vs. rising):
#'
#' - [download_topics()] / [download_topics_global()] — top related topics
#' - [download_themes()] / [download_themes_global()] — top related themes
#' - [download_topics_rising()] / [download_topics_rising_global()] — rising
#'   related topics
#' - [download_themes_rising()] / [download_themes_rising_global()] — rising
#'   related themes
#'
#' @details
#' **Prerequisites.** [initialize_python()] must be called before
#' `download_related()` to set up the Research API backend. [start_db()] must
#' also have been called; it connects to the database and populates
#' `gt.env$keywords_object` and `gt.env$time_object` from the tables
#' `batch_keywords` and `batch_time` (created via [add_keyword()]). These
#' in-memory objects are used to look up the keywords and time window for each
#' requested batch.
#'
#' **Dispatch.** `download_related()` is an S3 generic that dispatches on the
#' class of `object`. Passing a numeric scalar routes to the `.numeric` method,
#' which performs the actual download. Passing a numeric vector of length > 1
#' coerces `object` to a list and delegates to the `.list` method, which
#' iterates over batches sequentially. Passing a list directly also routes to
#' the `.list` method.
#'
#' **Download backend.** Requests are issued through the internal `.get_related()`
#' helper, which queries the Google Trends Research API via the Python backend.
#' One API call is made per keyword per location; results across keywords are
#' row-bound before being written to `data_related`.
#'
#' **Deduplication.** Before downloading, the function queries `data_related`
#' for locations already present for the requested batch (filtered by `topic`
#' and `rising`). Only locations not yet in the database are downloaded. If all
#' locations are already present, the function returns early with a message and
#' no requests are made.
#'
#' **Missing data.** If the API returns no data for a location (e.g. due to
#' insufficient search volume), the result for that location is skipped
#' (nothing is written to `data_related`) and a "No data returned" message is
#' emitted.
#'
#' @param object Numeric scalar, numeric vector, or list of numeric scalars.
#'   The object batch id(s) to download. A scalar downloads a single batch; a
#'   vector or list downloads multiple batches sequentially. Must refer to
#'   batches already registered via [add_keyword()].
#'
#' @param locations Character vector of ISO 3166-1 alpha-2 location codes.
#'   Defaults to `gt.env$countries` when set by [start_db()]; otherwise falls
#'   back to `globaltrends::countries`. Pass `"world"` (or use the `_global`
#'   convenience wrappers) to download the worldwide aggregate instead of
#'   country-level data.
#'
#' @param topic Logical scalar. `TRUE` to download related *topics*; `FALSE` to
#'   download related *themes*. The convenience wrappers set this automatically.
#'
#' @param rising Logical scalar. `TRUE` to download *rising* (breakout) results;
#'   `FALSE` to download *top* results. The convenience wrappers set this
#'   automatically.
#'
#' @return
#' Invisibly returns `TRUE`. The function is called for its side effects:
#' downloaded rows are appended to `data_related` in the active database, and
#' one progress message is emitted per location indicating whether data was
#' written or no data was returned.
#'
#' @seealso
#' [initialize_python()] to set up the Python backend (required before use).
#' [start_db()] to connect to the database and populate `gt.env`.
#' [add_keyword()] to register object batches before downloading.
#' [download_object()] to download raw search volume data for object keywords.
#'
#' @examples
#' \dontrun{
#' # Download top related topics for one object batch across all countries
#' download_topics(object = 1, locations = countries)
#'
#' # Download rising related themes for several batches sequentially
#' download_themes_rising(object = as.list(1:5), locations = countries)
#'
#' # Download top related topics worldwide
#' download_topics_global(object = 1)
#' }
#'
#' @export
#' @rdname download_related

download_related <- function(
  object,
  locations = NULL,
  topic = NULL,
  rising = NULL
) {
  UseMethod("download_related", object)
}

# ---- Public convenience wrappers --------------------------------------------

#' @export
#' @rdname download_related

download_topics <- function(object, locations = NULL) {
  download_related(
    object = object,
    locations = locations,
    topic = TRUE,
    rising = FALSE
  )
}

#' @export
#' @rdname download_related

download_themes <- function(object, locations = NULL) {
  download_related(
    object = object,
    locations = locations,
    topic = FALSE,
    rising = FALSE
  )
}

#' @export
#' @rdname download_related

download_topics_rising <- function(object, locations = NULL) {
  download_related(
    object = object,
    locations = locations,
    topic = TRUE,
    rising = TRUE
  )
}

#' @export
#' @rdname download_related

download_themes_rising <- function(object, locations = NULL) {
  download_related(
    object = object,
    locations = locations,
    topic = FALSE,
    rising = TRUE
  )
}

#' @export
#' @rdname download_related

download_topics_global <- function(object) {
  download_related(
    object = object,
    locations = "world",
    topic = TRUE,
    rising = FALSE
  )
}

#' @export
#' @rdname download_related

download_themes_global <- function(object) {
  download_related(
    object = object,
    locations = "world",
    topic = FALSE,
    rising = FALSE
  )
}

#' @export
#' @rdname download_related

download_topics_rising_global <- function(object) {
  download_related(
    object = object,
    locations = "world",
    topic = TRUE,
    rising = TRUE
  )
}

#' @export
#' @rdname download_related

download_themes_rising_global <- function(object) {
  download_related(
    object = object,
    locations = "world",
    topic = FALSE,
    rising = TRUE
  )
}

# ---- S3 methods -------------------------------------------------------------

#' @rdname download_related
#' @method download_related numeric
#' @export

download_related.numeric <- function(
  object,
  locations = NULL,
  topic = NULL,
  rising = NULL
) {
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

  .check_input(topic, "logical")
  .check_length(topic, 1)
  .check_input(rising, "logical")
  .check_length(rising, 1)

  # Vector input: delegate to list method for consistent iteration semantics.
  if (length(object) > 1) {
    download_related(
      object = as.list(object),
      locations = locations,
      topic = topic,
      rising = rising
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
  existing_locations <- .get_full(
    table = "data_related",
    in_batch_c = NULL,
    in_batch_o = object,
    in_topic = topic,
    in_rising = rising
  )
  loc_remaining <- locations[!(locations %in% existing_locations)]

  if (length(loc_remaining) == 0) {
    message(paste0(
      "No new locations to download | object: ", object,
      " | topic: ", topic,
      " | rising: ", rising, "."
    ))
    return(invisible(TRUE))
  }

  n_locs <- length(loc_remaining)
  for (i in seq_along(loc_remaining)) {
    loc <- loc_remaining[i]
    # .get_related() treats location = NULL as global; "world" maps to NULL.
    geo <- if (identical(loc, "world")) NULL else loc

    out <- do.call(rbind, lapply(terms_obj, function(t) {
      .get_related(
        location = geo,
        term = t,
        start_date = start_date,
        end_date = end_date,
        topic = topic,
        rising = rising
      )
    }))

    if (!is.null(out) && nrow(out) > 0) {
      out$batch_o <- object
      gt.env$dt_related <- data.table::rbindlist(
        list(gt.env$dt_related, data.table::setDT(out)),
        use.names = TRUE
      )
      message(paste0(
        "Downloaded related data | object: ", object,
        " | location: ", loc,
        " | topic: ", topic,
        " | rising: ", rising,
        " [", i, "/", n_locs, "]"
      ))
    } else {
      message(paste0(
        "No data returned | object: ", object,
        " | location: ", loc,
        " | topic: ", topic,
        " | rising: ", rising,
        " [", i, "/", n_locs, "]"
      ))
    }
  }

  invisible(TRUE)
}

#' @rdname download_related
#' @method download_related list
#' @export

download_related.list <- function(
  object,
  locations = NULL,
  topic = NULL,
  rising = NULL
) {
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")

  for (o in object) {
    download_related(o, locations = locations, topic = topic, rising = rising)
  }
  invisible(TRUE)
}
