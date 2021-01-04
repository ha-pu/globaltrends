#' @title Add sets of locations
#'
#' @description
#' The function adds a new set of locations for downloads and computations to
#' the database. The location set serves as input for all download and
#' computation functions.
#'
#' @details
#' Location sets control the loations for which data is downloaded or to which
#' computations are applied. By adding new location sets, the default sets
#' \emph{countries} and \emph{us_states} can be expanded by additional sets.
#' Thereby, users can compute DOI within a region (e.g. adding EU countries as
#' a set) or single countries (e.g. adding regions of France as a set). Download
#' and computation functions check whether data for a location already exists.
#' Therefore, data will not be duplicated when location data already exists from
#' another set.
#'
#' @param locations Locations that should be added as set of locations. Vector of
#' type \code{character}.
#' @param type Name of the location set that should be added. Object of type
#' \code{character} of length 1.
#' @param export Indicator whether the new location set should be directly
#' exported to .GlobalEnv. Object of type \code{logical}, defaults to
#' \code{TRUE}.
#' @param db Connection to database file that should be closed. Defaults
#' to \code{globaltrends_db}.
#'
#' @return
#' Message that the location set has been created successfully. Location data is
#' written to table \emph{data_locations}.
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
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom tibble tibble

add_locations <- function(locations, type, export = TRUE, db = globaltrends_db) {
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
    if (!(.x %in% codes)) stop(glue("Error: Invalid input for new location!\nLocation must be part of columns 'country_code' or 'sub_code' of table gtrendsR::countries.\nYou provided {.x}."))
  })

  data <- tibble(location = locations, type = type)
  dbWriteTable(conn = db, name = "data_locations", value = data, append = TRUE)

  if (export) .export_locations()

  message(glue("Successfully created new location set {type} ({locations_collapse}).", locations_collapse = paste(locations, collapse = ", ")))
}

#' @title Export locations to .GlobalEnv
#'
#' @keywords internal
#' @noRd

.export_locations <- function() {
  locations <- distinct(.tbl_locations, type)
  locations <- collect(locations)
  locations <- locations$type

  lst_locations <- map(locations, ~ {
    out <- filter(.tbl_locations, .data$type == .x)
    out <- collect(out)
    out <- pull(out, .data$location)
    return(out)
  })

  names(lst_locations) <- locations

  invisible(list2env(lst_locations, envir = .GlobalEnv))
}
