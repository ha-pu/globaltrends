#' @title Add sets of locations
#'
#' @description
#'
#' @details
#'
#' @param locations xxx.
#' @param type xxx.
#' @param export xxx.
#' @param db Connection to database file that should be closed. Defaults
#' to \code{globaltrends_db}.
#'
#' @return
#' 
#' @example
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
#' @importFrom rlang .data
#' @importFrom tibble tibble

add_locations <- function(locations, type, export = TRUE, db = globaltrends_db) {
  data <- tibble(location = locations, type = type)
  dbWriteTable(conn = db, name = "data_locations", value = data, append = TRUE)
  
  if (export) .export_locations()
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