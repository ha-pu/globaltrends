#' @title Add a location set
#'
#' @description
#' Adds a user-defined *location set* to the `data_locations` database table.
#' Location sets are used as inputs to download and computation functions.
#'
#' @details
#' Location sets control which locations are downloaded or included in
#' computations. The package ships with default sets (e.g., `countries`,
#' `us_states`), and you can expand these by adding your own sets (e.g., "EU",
#' "DACH", or subnational regions for a country).
#'
#' The function is designed to be idempotent with respect to `(type, location)`:
#' if a location already exists for a given `type`, it will be skipped (not
#' duplicated).
#'
#' @section Known API limitation:
#' The Google Trends API cannot handle the location code `"NA"` (Namibia).
#' If `"NA"` is supplied, it will be dropped. If `"NA"` is the only supplied
#' location, the function errors.
#'
#' @param locations Character vector of location codes to add. Codes must be
#'   present in `gtrendsR::countries$country_code` or
#'   `gtrendsR::countries$sub_code`. Duplicates are removed.
#'
#' @param type Character scalar. Name of the location set to which `locations`
#'   should be added (e.g., `"DACH"`, `"EU"`).
#'
#' @param export Logical scalar. If `TRUE` (default), the package environment
#'   `gt.env` is refreshed so the new/updated location set becomes available
#'   immediately.
#'
#' @return
#' Invisibly returns a tibble of rows that were appended to the database
#' (columns: `location`, `type`). A message is emitted summarizing what happened.
#'
#' @examples
#' \dontrun{
#' add_locations(locations = c("AT", "CH", "DE"), type = "DACH")
#' }
#'
#' @export
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr collect distinct filter pull
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom tibble tibble

add_locations <- function(locations, type, export = TRUE) {
  # --- validate inputs --------------------------------------------------------
  .check_input(locations, "character")
  .check_input(type, "character")
  .check_length(type, 1)
  .check_input(export, "logical")
  .check_length(export, 1)

  if (length(locations) == 0) {
    stop("`locations` must contain at least one location code.", call. = FALSE)
  }
  if (any(is.na(locations))) {
    stop("`locations` must not contain NA values.", call. = FALSE)
  }

  # Normalize: trim whitespace and drop duplicates early
  locations <- unique(trimws(locations))

  # --- validate codes against gtrendsR dictionary ----------------------------
  codes <- unique(na.omit(c(
    gtrendsR::countries$country_code,
    gtrendsR::countries$sub_code
  )))
  invalid <- setdiff(locations, codes)

  if (length(invalid) > 0) {
    stop(
      paste0(
        "Invalid location code(s): ",
        paste(invalid, collapse = ", "),
        ". Valid codes must appear in `gtrendsR::countries$country_code` or `gtrendsR::countries$sub_code`."
      ),
      call. = FALSE
    )
  }

  # --- handle Namibia limitation ---------------------------------------------
  if ("NA" %in% locations) {
    locations <- locations[locations != "NA"]

    if (length(locations) == 0) {
      stop(
        paste0(
          "The Google Trends API cannot handle the location code 'NA' (Namibia). ",
          "It was dropped, leaving `locations` empty."
        ),
        call. = FALSE
      )
    } else {
      warning(
        "The Google Trends API cannot handle the location code 'NA' (Namibia). It was dropped.",
        call. = FALSE
      )
    }
  }

  # --- avoid duplicates in the database --------------------------------------
  # Only append (type, location) pairs that do not already exist.
  if (is.null(gt.env$tbl_locations)) {
    already_present <- NULL
  } else {
    in_type <- type
    existing <- gt.env$tbl_locations |>
      filter(.data$type == in_type & .data$location %in% locations) |>
      collect()
    already_present <- existing$location
  }
  to_add <- setdiff(locations, already_present)

  if (length(to_add) == 0) {
    if (export) {
      .export_locations()
    }
    message(
      paste0(
        "No new locations added for set '",
        type,
        "'. ",
        "All provided locations already exist (",
        paste(locations, collapse = ", "),
        ")."
      )
    )
    return(invisible(tibble(location = character(), type = character())))
  }

  data_to_add <- tibble(location = to_add, type = type)
  dbAppendTable(
    conn = gt.env$globaltrends_db,
    name = "data_locations",
    value = data_to_add
  )

  if (export) {
    .export_locations()
  }

  # --- user feedback ----------------------------------------------------------
  if (length(already_present) > 0) {
    message(
      paste0(
        "Location set '",
        type,
        "': added ",
        length(to_add),
        " location(s) (",
        paste(to_add, collapse = ", "),
        "); skipped ",
        length(already_present),
        " existing (",
        paste(already_present, collapse = ", "),
        ")."
      )
    )
  } else {
    message(
      paste0(
        "Successfully created/extended location set '",
        type,
        "' with ",
        length(to_add),
        " location(s) (",
        paste(to_add, collapse = ", "),
        ")."
      )
    )
  }

  invisible(data_to_add)
}

#' @title Export location sets to `gt.env`
#'
#' @description
#' Loads all distinct `(type, location)` pairs from the database-backed locations
#' table and exposes each set as a character vector in the package environment
#' `gt.env`, named by its `type`.
#'
#' @details
#' This internal helper performs a single database read (collect once) and then
#' splits the result in memory for performance and reproducibility.
#'
#' @return
#' Invisibly returns the list of location vectors that were assigned into `gt.env`.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr collect distinct
#' @importFrom rlang .data

.export_locations <- function() {
  df <- gt.env$tbl_locations |>
    distinct(.data$type, .data$location) |>
    collect()

  # Split to a named list: each element is a character vector of locations
  lst_locations <- split(df$location, df$type)

  # Assign into package environment
  invisible(list2env(lst_locations, envir = gt.env))
}
