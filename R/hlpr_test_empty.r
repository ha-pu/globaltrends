#' @title Test wheter table has entries for given criteria
#'
#' @keywords internal
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom utils head

.test_empty <- function(table, batch_c = NULL, batch_o = NULL, location = NULL, locations = NULL) {
  if (is.character(table)) {
    in_batch_c <- batch_c
    in_batch_o <- batch_o
    in_location <- location
    in_locations <- locations
    if (table == "data_control") {
      out <- filter(.tbl_control, batch == in_batch_c & location == in_location)
    } else if (table == "data_object") {
      out <- filter(.tbl_object, batch == in_batch_o & location == in_location)
    } else if (table == "data_mapping") {
      out <- filter(.tbl_mapping, batch_c == in_batch_c & batch_o == in_batch_o & location == in_location)
    } else if (table == "data_score") {
      out <- filter(.tbl_score, batch_c == in_batch_c & batch_o == in_batch_o & location == in_location)
    } else if (table == "data_doi") {
      out <- filter(.tbl_doi, batch_c == in_batch_c & batch_o == in_batch_o & locations == in_locations)
    } else if (table == "data_global") {
      out <- filter(.tbl_global, batch == in_batch_o)
    }
    out <- head(out)
    out <- collect(out)
    out <- nrow(out)
    out <- out == 0
    return(out)
  } else {
    stop("Error: 'table' must be an object of type character.\nYou supplied an object of a different type.")
  }
}
