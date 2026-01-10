#' @title Download data for control keyword batches
#'
#' @aliases download_control download_control.numeric download_control.list
#'
#' @description
#' Downloads Google Trends search volumes for one or more *control* batches
#' across a set of locations and writes the results to the database table
#' `data_control`.
#'
#' @details
#' Control batches (up to five keywords per batch) and their time windows are
#' defined in the tables `batch_keywords` and `batch_time` (typically created
#' via [add_keyword()]). This function retrieves the relevant keywords and the
#' batch-specific time window from `gt.env$keywords_control` and
#' `gt.env$time_control`.
#'
#' Downloads are performed through the package internal `.get_trend()` helper.
#' Depending on configuration, `.get_trend()` may use `gtrendsR::gtrends()` or
#' the Research API backend initialized via [initialize_python()].
#'
#' The function avoids duplicate downloads: it checks which locations already
#' exist for the requested control batch in `data_control` and only downloads
#' missing locations.
#'
#' @section Category codes:
#' Avoid category codes unless you are confident they apply uniformly to all
#' keywords in the batch. Google Trends applies a category constraint to the
#' entire request, which can unintentionally change the meaning of control and
#' object keywords.
#'
#' @param control Numeric scalar/vector or list of numeric scalars. Control
#'   batch id(s) to download.
#'
#' @param locations Character vector of location codes. Defaults to
#'   `gt.env$countries` when available; otherwise `globaltrends::countries`.
#'   Use `""` to request the global aggregate (`"world"`).
#'
#' @return
#' Invisibly returns `TRUE`. Called for its side effects (writing to
#' `data_control`) and emits a message per location.
#'
#' @examples
#' \dontrun{
#' download_control(control = 1, locations = countries)
#' download_control(control = as.list(1:5), locations = countries)
#' download_control_global(control = 1)
#' }
#'
#' @export
#' @rdname download_control
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr mutate
#' @importFrom purrr walk
#' @importFrom rlang .data

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
  existing <- .get_full(table = "data_control", batch_c = control)
  loc_remaining <- locations[!(locations %in% existing)]

  if (length(loc_remaining) == 0) {
    message(paste0("No new locations to download | control: ", control, "."))
    return(invisible(TRUE))
  }

  walk(
    seq_along(loc_remaining),
    ~ {
      loc <- loc_remaining[[.x]]
      in_location <- ifelse(identical(loc, ""), "world", loc)

      # Global download: when using gtrendsR, a blank geo typically indicates global.
      # For the Research API backend, we call without location (implementation-specific).
      out <- if (identical(in_location, "world")) {
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
        out <- dplyr::mutate(out, batch = control)
        dbAppendTable(
          conn = gt.env$globaltrends_db,
          name = "data_control",
          value = out
        )
      }

      message(paste0(
        "Downloaded control data | control: ",
        control,
        " | location: ",
        in_location,
        " [",
        .x,
        "/",
        length(loc_remaining),
        "]"
      ))
    }
  )

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

  walk(control, download_control, locations = locations)
  invisible(TRUE)
}

#' @title Download global control data
#'
#' @description
#' Convenience wrapper around [download_control()] to download global (world)
#' control series. Internally this is implemented by passing `locations = ""`.
#'
#' @param control Numeric scalar/vector or list. Control batch id(s) to download.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
#' @rdname download_control

download_control_global <- function(control) {
  download_control(control = control, locations = "")
}
