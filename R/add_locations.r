#' @example
#' \dontrun{
#' add_locations(locations = c("AT", "CH", "DE"), type = "DACH")
#' }

add_locations <- function(locations, type, export = TRUE, db = globaltrends_db) {
  data <- tibble::tibble(location = locations, type = type)
  DBI::dbWriteTable(conn = db, name = "data_locations", value = data, append = TRUE)
  
  if (export) .export_locations()
}


.export_locations <- function() {
  locations <- dplyr::distinct(.tbl_locations, type)
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