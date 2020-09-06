run_map <- function(control, object, lst_geo = lst_wdi) {
  purrr::map(lst_geo, ~{
    if (.test_empty(table = "data_map", batch_c = control, batch_o = object, geo = .x)){
      qry_con <- dplyr::filter(data_con, batch == control & geo == .x)
      qry_con <- dplyr::collect(qry_con)
      qry_obj <- dplyr::filter(data_obj, batch == object & geo == .x)
      qry_obj <- dplyr::collect(qry_obj)
      if (nrow(qry_con) > 0 & nrow(qry_obj) > 0) {
        term_con <- stats::aggregate(qry_con$hits, list(qry_con$keyword), mean)
        term_con <- term_con$Group.1[order(term_con$x)]
        term_obj <- stats::aggregate(qry_obj$hits, list(qry_obj$keyword), mean)
        term_obj <- term_obj$Group.1[term_obj$x == max(term_obj$x)]
        date_min <- lubridate::as_date(max(min(qry_con$date), min(qry_obj$date)))
        date_max <- lubridate::as_date(min(max(qry_con$date), max(qry_obj$date)))
        if (date_min < date_max) {
          i <- 1
          while (i <= 5) {
            out <- .get_trend(geo = .x, term = c(term_con[[i]], term_obj[[1]]), time = str_c(date_min, date_max, sep = " "))
            if (!is.null(out) & median(out$hits[out$keyword == term_con[[i]]]) > 1) {
              out <- dplyr::mutate(out, batch_c = control, batch_o = object)
              DBI::dbWriteTable(conn = gtrends_db, name = "data_map", value = out, append = TRUE)
              break()
            }
            i <- i + 1
          }
        }
      }
    }
    message(stringr::str_c("run_map | control: ", control, " | object: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
