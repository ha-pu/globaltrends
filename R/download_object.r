#' @title Download data for object keyword batches
#'
#' @aliases download_object download_object.numeric download_object.list
#'
#' @description
#' Downloads Google Trends search volumes for one or more *object* batches
#' (`batch_o`) together with a single control keyword (from `batch_c`) used for
#' scaling/mapping, across a set of locations. Results are written to the
#' database table `data_object`.
#'
#' @details
#' Each object batch contains up to four object keywords. For each download
#' request, this function prepends exactly one control keyword to the query so
#' that object hits can be mapped to the control scale used elsewhere in the
#' package.
#'
#' The function selects the control keyword dynamically per location:
#' it inspects existing `data_control` for the chosen control batch and location,
#' ranks control keywords by their average `hits`, and tries them in ascending
#' order until a control keyword yields non-zero signal in the returned series.
#' This reduces the likelihood of failed requests caused by zero-signal control
#' terms.
#'
#' The function avoids duplicate downloads by skipping locations already present
#' for `(batch_c, batch_o)` in `data_object`.
#'
#' Downloads are performed through the package internal `.get_trend()` helper,
#' which may use `gtrendsR::gtrends()` or the Research API backend initialized
#' via [initialize_python()].
#'
#' @section Category codes:
#' Avoid category codes unless you are confident they apply uniformly to all
#' keywords in the batch. Google Trends applies a category constraint to the
#' entire request, which can unintentionally change the meaning of control and
#' object keywords.
#'
#' @param object Numeric scalar/vector or list of numeric scalars. Object batch
#'   id(s) (`batch_o`) to download.
#'
#' @param control Numeric scalar. Control batch id (`batch_c`) used for mapping.
#'   Defaults to `1`.
#'
#' @param locations Character vector of location codes. Defaults to
#'   `gt.env$countries` when available; otherwise `globaltrends::countries`.
#'   Use `""` to request the global aggregate (`"world"`).
#'
#' @return
#' Invisibly returns `TRUE`. Called for its side effects (writing to
#' `data_object`) and emits messages per location.
#'
#' @examples
#' \dontrun{
#' download_object(object = 1, control = 1, locations = countries)
#' download_object(object = as.list(1:5), control = 1, locations = countries)
#' download_object_global(object = 1, control = 1)
#' }
#'
#' @export
#' @rdname download_object
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr collect filter mutate summarise
#' @importFrom purrr walk
#' @importFrom rlang .data

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

  walk(
    seq_along(loc_remaining),
    ~ {
      loc <- loc_remaining[[.x]]
      in_location <- ifelse(identical(loc, ""), "world", loc)

      # We require `data_control` for the same control batch and location to
      # pick an appropriate control keyword for mapping.
      qry_control <- gt.env$tbl_control |>
        filter(.data$batch == control, .data$location == in_location) |>
        collect()

      if (nrow(qry_control) == 0) {
        message(paste0(
          "Skipped object download (missing control baseline) | object: ",
          object,
          " | control: ",
          control,
          " | location: ",
          in_location,
          "."
        ))
        return(invisible(NULL))
      }

      # Rank control keywords by average hits (ascending) and keep only those with signal.
      # We try control keywords with lower average hits first to reduce saturation risk.
      terms_con <- qry_control |>
        summarise(hits = mean(.data$hits, na.rm = TRUE), .by = .data$keyword) |>
        filter(.data$hits > 0)

      if (nrow(terms_con) == 0) {
        stop(
          paste0(
            "Too little signal in control batch ",
            control,
            " for location ",
            in_location,
            ". ",
            "Reconsider choice of control keywords."
          ),
          call. = FALSE
        )
      }

      terms_con <- terms_con$keyword[order(terms_con$hits)]

      # Try control keywords until one returns non-zero signal in the result.
      success <- FALSE
      out <- NULL

      for (term_c in terms_con) {
        if (identical(in_location, "world")) {
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
            in_location,
            "). ",
            "Reconsider control keywords or time window."
          ),
          call. = FALSE
        )
      }

      # Persist data
      out <- mutate(out, batch_c = control, batch_o = object)
      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "data_object",
        value = out
      )

      message(paste0(
        "Downloaded object data | object: ",
        object,
        " | control: ",
        control,
        " | location: ",
        in_location,
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

#' @rdname download_object
#' @method download_object list
#' @export

download_object.list <- function(object, control = 1, locations = NULL) {
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

  walk(object, download_object, control = control, locations = locations)
  invisible(TRUE)
}

#' @title Download global object data
#'
#' @description
#' Convenience wrapper around [download_object()] to download global (world)
#' object series. Internally this is implemented by passing `locations = ""`.
#'
#' @param object Numeric scalar/vector or list. Object batch id(s) to download.
#' @param control Numeric scalar. Control batch id used for mapping. Defaults to `1`.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @export
#' @rdname download_object

download_object_global <- function(object, control = 1) {
  download_object(object = object, control = control, locations = "")
}
