#' @title Add a location set
#'
#' @description
#' Adds location codes to a named *location set* in the `data_locations`
#' database table. A location set is a named group of codes (e.g.,
#' `"countries"`, `"DACH"`) that is passed as the `locations` argument to
#' download and computation functions. After insertion the set is immediately
#' accessible as `gt.env$<type>`.
#'
#' @details
#' The package ships with two default sets — `"countries"` and `"us_states"` —
#' written to the database by [start_db()]. Use `add_locations()` to define
#' additional sets such as `"EU"`, `"DACH"`, or subnational regions for a
#' specific country.
#'
#' The function is idempotent with respect to `(type, location)` pairs: codes
#' that already exist in the named set are silently skipped, so repeated calls
#' are safe. Leading and trailing whitespace is trimmed from all codes before
#' validation and insertion.
#'
#' @section Known API limitation:
#' The Google Trends API cannot handle the location code `"NA"` (Namibia). If
#' `"NA"` is supplied it is dropped with a warning. If it is the only code
#' supplied, the function errors.
#'
#' @param locations Character vector of location codes to add. Each code must
#'   appear in `gtrendsR::countries$country_code` (country level) or
#'   `gtrendsR::countries$sub_code` (subnational level). Leading/trailing
#'   whitespace and duplicates are removed automatically.
#'
#' @param type Character scalar. Name of the location set to which `locations`
#'   should be added (e.g., `"DACH"`, `"EU"`). After export the set is
#'   available as `gt.env$<type>`.
#'
#' @param export Logical scalar. If `TRUE` (default), `gt.env` is refreshed so
#'   the updated set is available immediately. Set to `FALSE` when calling
#'   `add_locations()` several times in sequence to avoid a redundant database
#'   read after each call; run `add_locations(..., export = TRUE)` on the final
#'   call, or restart the session, to make all sets available.
#'
#' @return
#' Invisibly returns a tibble of the rows appended to `data_locations` (columns:
#' `location`, `type`). Returns a zero-row tibble when all supplied codes
#' already exist in the set. A message is emitted in either case summarising
#' how many codes were added and how many were skipped.
#'
#' @examples
#' \dontrun{
#' # Create a custom set for the DACH region
#' add_locations(locations = c("AT", "CH", "DE"), type = "DACH")
#'
#' # Add subnational codes (US states from the built-in vector)
#' add_locations(locations = us_states, type = "us_states")
#'
#' # Add several sets without redundant DB reads; refresh once at the end
#' add_locations(locations = c("AT", "CH", "DE"), type = "DACH", export = FALSE)
#' add_locations(locations = c("BE", "LU", "NL"), type = "benelux", export = TRUE)
#' }
#'
#' @seealso
#' * [download_control()] and [download_object()] — pass a location set here
#' * [compute_score()] and [compute_doi()] — pass a location set here
#' * [countries] and [us_states] — built-in location vectors
#' * [start_db()] — populates the default `"countries"` and `"us_states"` sets
#' * [gtrendsR::countries] — source of all valid location codes
#'
#' @export
#' @importFrom stats na.omit

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
  if (is.null(gt.env$dt_locations)) {
    already_present <- NULL
  } else {
    dt <- gt.env$dt_locations
    # `target_type` avoids colliding with the `type` column of `dt`: under
    # data.table's NSE (see `.datatable.aware` in zzz.r), a bare symbol that
    # matches a column name resolves to the COLUMN, not this argument, which
    # would make the filter always-true (`dt$type == dt$type`).
    target_type <- type
    already_present <- dt[dt$type == target_type & dt$location %in% locations, ]$location
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
    return(invisible(data.frame(location = character(), type = character(), stringsAsFactors = FALSE)))
  }

  data_to_add <- data.table::data.table(location = to_add, type = type)
  gt.env$dt_locations <- data.table::rbindlist(
    list(gt.env$dt_locations, data_to_add),
    use.names = TRUE
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
#' Reads all distinct `(type, location)` pairs from the database-backed
#' `data_locations` table and assigns each set as a named character vector
#' into the package environment `gt.env`. A single `collect()` is performed and
#' then split in memory, so the cost is one DB round-trip regardless of how
#' many sets exist.
#'
#' Called automatically by [add_locations()] when `export = TRUE`.
#'
#' @return
#' Invisibly returns the named list of character vectors assigned into `gt.env`.
#'
#' @keywords internal
#' @noRd

.export_locations <- function() {
  dt <- gt.env$dt_locations
  if (is.null(dt) || nrow(dt) == 0L) {
    return(invisible(NULL))
  }
  dt <- unique(dt[order(dt$type, dt$location), ])
  lst_locations <- split(dt$location, dt$type)
  invisible(list2env(lst_locations, envir = gt.env))
}
