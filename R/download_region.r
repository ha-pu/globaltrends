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
#' This function requires the Research API backend (Python) to be initialized via
#' [initialize_python()]. It uses the internal `.get_region()` helper to fetch
#' regional interest for each object keyword in the specified batch and for each
#' requested location.
#'
#' The function avoids duplicate downloads: it checks which locations already
#' exist for the requested object batch in `data_region` and only downloads
#' missing locations.
#'
#' Location semantics:
#' - `""` or `NULL` indicates the global aggregate and is reported as `"world"`
#'   in messages and output.
#'
#' @param object Numeric scalar/vector or list of numeric scalars. Object batch
#'   id(s) (`batch_o`) to download.
#'
#' @param locations Character vector of location codes. Defaults to
#'   `gt.env$countries` when available; otherwise `globaltrends::countries`.
#'   Use `""` to request global data (`"world"`).
#'
#' @return
#' Invisibly returns `TRUE`. Called for its side effects (writing to
#' `data_region`) and emits a message per location.
#'
#' @examples
#' \dontrun{
#' initialize_python(api_key = "XXX", conda_env = "/path/to/env")
#' start_db()
#' download_region(object = 1, locations = countries)
#' download_region(object = as.list(1:3), locations = countries)
#' download_region_global(object = 1)
#' }
#'
#' @export
#' @rdname download_region
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfr walk
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

  walk(
    seq_along(loc_remaining),
    ~ {
      loc_in <- loc_remaining[[.x]]
      is_global <- is.null(loc_in) || identical(loc_in, "")
      loc <- if (is_global) "world" else loc_in

      # Download region series per keyword; bind rows across keywords.
      out <- if (is_global) {
        map_dfr(
          terms_obj,
          ~ .get_region(term = .x, start_date = start_date, end_date = end_date)
        )
      } else {
        map_dfr(
          terms_obj,
          ~ .get_region(
            location = loc_in,
            term = .x,
            start_date = start_date,
            end_date = end_date
          )
        )
      }

      if (is.null(out) || nrow(out) == 0) {
        message(paste0(
          "No region data returned | object: ",
          object,
          " | location: ",
          loc,
          " [",
          .x,
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
        .x,
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

  walk(object, download_region, locations = locations)
  invisible(TRUE)
}

#' @title Download global regional interest data
#'
#' @description
#' Convenience wrapper around [download_region()] to download global (world)
#' regional interest data. Internally implemented by passing `locations = ""`.
#'
#' @param object Numeric scalar/vector or list. Object batch id(s) to download.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
#' @rdname download_region

download_region_global <- function(object) {
  download_region(object = object, locations = "")
}
