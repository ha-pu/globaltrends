#' @title Add sets of locations
#'
#' @description
#' The function adds a new set of locations for downloads and computations to
#' the database. The location set serves as input for all download and
#' computation functions.
#'
#' @details
#' Location sets control the locations for which data is downloaded or to which
#' computations are applied. By adding new location sets, the default sets
#' *countries* and *us_states* can be expanded by additional sets.
#' Thereby, users can compute DOI within a region (e.g., adding EU countries as
#' a set) or single countries (e.g., adding regions of France as a set). Download
#' and computation functions check whether data for a location already exists.
#' Therefore, data will not be duplicated when location data already exists from
#' another set.
#' Unfortunately, the Google Trends API cannot handle the location
#' "NA - Namibia". Therefore, the location will be dropped automatically.
#'
#' @param locations Locations that should be added as set of locations. Vector of
#' type `character`.
#' @param type Name of the location set that should be added. Object of type
#' `character` of length 1.
#' @param export Indicator whether the new location set should be directly
#' exported to the package environment `gt.env`. Object of type `logical`,
#' defaults to `TRUE`.
#'
#' @return
#' Message that the location set has been created successfully. Location data is
#' written to table *data_locations*.
#'
#' @examples
#' \dontrun{
#' add_locations(locations = c("AT", "CH", "DE"), type = "DACH")
#' }
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom purrr map
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom tibble tibble

add_locations <- function(locations, type, export = TRUE) {
  .check_input(locations, "character")
  .check_input(type, "character")
  .check_length(type, 1)
  .check_input(export, "logical")
  .check_length(export, 1)

  # check new locations
  codes <- c(gtrendsR::countries$country_code, gtrendsR::countries$sub_code)
  codes <- unique(codes)
  codes <- na.omit(codes)
  walk(locations, ~ {
    if (!(.x %in% codes)) stop(paste0("Error: Invalid input for new location!\nLocation must be part of columns 'country_code' or 'sub_code' of table gtrendsR::countries.\nYou provided ", .x, "."))
  })

  # handle Namibia
  if (any(locations == "NA")) {
    locations <- locations[locations != "NA"]

    if (length(locations) == 0) {
      stop("Unfortunately, the Google Trends API cannot handle the location 'NA - Namibia'. The location 'NA' has been dropped.\nThe argument 'locations' now has lenght 0!")
    } else {
      warning("Unfortunately, the Google Trends API cannot handle the location 'NA - Namibia'. The location 'NA' has been dropped.")
    }
  }

  data <- tibble(location = locations, type = type)
  dbWriteTable(conn = gt.env$globaltrends_db, name = "data_locations", value = data, append = TRUE)

  if (export) .export_locations()

  message(paste0("Successfully created new location set ", type, " (", paste(locations, collapse = ", "), ")."))
}

#' @title Export locations to package environment gt.env
#'
#' @keywords internal
#' @noRd

.export_locations <- function() {
  locations <- distinct(gt.env$tbl_locations, .data$type)
  locations <- collect(locations)
  locations <- locations$type

  lst_locations <- map(locations, ~ {
    out <- filter(gt.env$tbl_locations, .data$type == .x)
    out <- collect(out)
    out <- pull(out, .data$location)
    return(out)
  })

  names(lst_locations) <- locations

  invisible(list2env(lst_locations, envir = gt.env))
}
