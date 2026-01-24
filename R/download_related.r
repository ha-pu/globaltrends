#' @title Download related topics and themes (including rising variants)
#'
#' @description
#' Convenience wrappers around [download_related()] that download one of four
#' related-queries variants for the keywords in one or more object batches:
#'
#' - related topics (top)
#' - related themes (top)
#' - related topics (rising)
#' - related themes (rising)
#'
#' All downloads are written to the database table `data_related`.
#'
#' @details
#' This feature requires the Research API backend (Python) initialized via
#' [initialize_python()]. The internal `.get_related()` helper is used to query
#' related topics/themes for each keyword in the object batch and for each
#' requested location.
#'
#' The function avoids duplicate downloads by skipping locations already present
#' in `data_related` for the requested `batch_o` (optionally, you may want to
#' extend de-duplication to also include the `topic`/`rising` dimensions; see
#' notes in code).
#'
#' @param object Numeric scalar/vector or list of numeric scalars. Object batch
#'   id(s) (`batch_o`) to download.
#' @param locations Character vector of location codes. Defaults to
#'   `gt.env$countries` when available; otherwise `globaltrends::countries`.
#'   Use `""` to request global data (`"world"`).
#'
#' @return Invisibly returns `TRUE`. Called for its side effects (writing to
#'   `data_related`) and emits a message per location.
#'
#' @name download_related

NULL

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
    locations = "",
    topic = TRUE,
    rising = FALSE
  )
}

#' @export
#' @rdname download_related

download_themes_global <- function(object) {
  download_related(
    object = object,
    locations = "",
    topic = FALSE,
    rising = FALSE
  )
}

#' @export
#' @rdname download_related

download_topics_rising_global <- function(object) {
  download_related(object = object, locations = "", topic = TRUE, rising = TRUE)
}

#' @export
#' @rdname download_related

download_themes_rising_global <- function(object) {
  download_related(
    object = object,
    locations = "",
    topic = FALSE,
    rising = TRUE
  )
}

# ---- Core implementation ----------------------------------------------------

#' @rdname download_related
#' @keywords internal
#' @noRd
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfr walk
#' @importFrom rlang .data

download_related <- function(
  object,
  locations = NULL,
  topic = NULL,
  rising = NULL
) {
  UseMethod("download_related", object)
}

#' @rdname download_related
#' @method download_related numeric
#' @keywords internal
#' @noRd

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

  # -------------------------------------------------------------------------
  # De-duplication
  # -------------------------------------------------------------------------
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
      "No new locations to download | object: ",
      object,
      " | topic: ",
      topic,
      " | rising: ",
      rising,
      "."
    ))
    return(invisible(TRUE))
  }

  walk(
    seq_along(loc_remaining),
    ~ {
      loc_in <- loc_remaining[[.x]]
      is_global <- is.null(loc_in) || identical(loc_in, "")
      loc_label <- if (is_global) "world" else loc_in

      out <- if (is_global) {
        map_dfr(
          terms_obj,
          ~ .get_related(
            term = .x,
            start_date = start_date,
            end_date = end_date,
            topic = topic,
            rising = rising
          )
        )
      } else {
        map_dfr(
          terms_obj,
          ~ .get_related(
            location = loc_in,
            term = .x,
            start_date = start_date,
            end_date = end_date,
            topic = topic,
            rising = rising
          )
        )
      }

      if (is.null(out) || nrow(out) == 0) {
        message(paste0(
          "No related data returned | object: ",
          object,
          " | location: ",
          loc_label,
          " | topic: ",
          topic,
          " | rising: ",
          rising,
          " [",
          .x,
          "/",
          length(loc_remaining),
          "]"
        ))
        return(invisible(TRUE))
      }

      # Persist: include flags so downstream users can filter variants.
      out <- mutate(out, batch_o = object, topic = topic, rising = rising)

      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "data_related",
        value = out
      )

      message(paste0(
        "Downloaded related data | object: ",
        object,
        " | location: ",
        loc_label,
        " | topic: ",
        topic,
        " | rising: ",
        rising,
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

#' @rdname download_related
#' @method download_related list
#' @keywords internal
#' @noRd

download_related.list <- function(
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

  walk(
    object,
    download_related,
    locations = locations,
    topic = topic,
    rising = rising
  )
  invisible(TRUE)
}
