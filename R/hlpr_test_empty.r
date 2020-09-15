#' @title Test wheter table has entries for given criteria
#'
#' @keywords internal
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom utils head

.test_empty <- function(table, batch_c = NULL, batch_o = NULL, geo = NULL, locations = NULL) {
  if (is.character(table)) {
    in_batch_c <- batch_c
    in_batch_o <- batch_o
    in_geo <- geo
    in_locations <- locations
    if (table == "data_control") {
      out <- filter(data_control, batch == in_batch_c & geo == in_geo)
    } else if (table == "data_object") {
      out <- filter(data_object, batch == in_batch_o & geo == in_geo)
    } else if (table == "data_map") {
      out <- filter(data_map, batch_c == in_batch_c & batch_o == in_batch_o & geo == in_geo)
    } else if (table == "data_score") {
      out <- filter(data_score, batch_c == in_batch_c & batch_o == in_batch_o & geo == in_geo)
    } else if (table == "data_agg") {
      out <- filter(data_agg, batch_c == in_batch_c & batch_o == in_batch_o & locations == in_locations)
    } else if (table == "data_wrld") {
      out <- filter(data_wrld, batch == in_batch_o)
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
