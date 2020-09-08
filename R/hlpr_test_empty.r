# test whether table has entries for given criteria

.test_empty <- function(table, batch_c = NULL, batch_o = NULL, geo = NULL) {
  if (is.character(table)) {
    in_batch_c <- batch_c
    in_batch_o <- batch_o
    in_geo <- geo
    if (table == "data_con") {
      out <- dplyr::filter(data_con, batch == in_batch_c & geo == in_geo)
    } else if (table == "data_obj") {
      out <- dplyr::filter(data_obj, batch == in_batch_o & geo == in_geo)
    } else if (table == "data_map") {
      out <- dplyr::filter(data_map, batch_c == in_batch_c & batch_o == in_batch_o & geo == in_geo)
    } else if (table == "data_score") {
      out <- dplyr::filter(data_score, batch_c == in_batch_c & batch_o == in_batch_o & geo == in_geo)
    } else if (table == "data_agg") {
      out <- dplyr::filter(data_agg, batch_c == in_batch_c & batch_o == in_batch_o)
    } else if (table == "data_wrld") {
      out <- dplyr::filter(data_wrld, batch == in_batch_o)
    }
    out <- head(out)
    out <- dplyr::collect(out)
    out <- nrow(out)
    out <- out == 0
    return(out)
  }
}
