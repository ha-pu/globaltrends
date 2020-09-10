#' @title Test wheter table has entries for given criteria
#'
#' @keywords internal
#'
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom utils head

.test_empty <- function(table, batch_c = NULL, batch_o = NULL, geo = NULL, lst_geo = NULL) {
  if (is.character(table)) {
    in_batch_c <- batch_c
    in_batch_o <- batch_o
    in_geo <- geo
    in_lst_geo <- lst_geo
    if (table == "data_con") {
      out <- filter(data_con, batch == in_batch_c & geo == in_geo)
    } else if (table == "data_obj") {
      out <- filter(data_obj, batch == in_batch_o & geo == in_geo)
    } else if (table == "data_map") {
      out <- filter(data_map, batch_c == in_batch_c & batch_o == in_batch_o & geo == in_geo)
    } else if (table == "data_score") {
      out <- filter(data_score, batch_c == in_batch_c & batch_o == in_batch_o & geo == in_geo)
    } else if (table == "data_agg") {
      out <- filter(data_agg, batch_c == in_batch_c & batch_o == in_batch_o & lst_geo == in_lst_geo)
    } else if (table == "data_wrld") {
      out <- filter(data_wrld, batch == in_batch_o)
    }
    out <- head(out)
    out <- collect(out)
    out <- nrow(out)
    out <- out == 0
    return(out)
  }
}
