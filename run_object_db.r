run_object <- function(object, lst_geo = lst_wdi) {
  terms <- terms_obj$keyword[terms_obj$batch == object]
  time <- time_obj$time[time_obj$batch == object]
  map(lst_geo, ~{
    if (.test_empty(table = "data_obj", batch_o = object, geo = .x)){
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (!is.null(out)) {
        out <- mutate(out, batch = object)
        dbWriteTable(conn = gtrends_db, name = "data_obj", value = out, append = TRUE)
      }
    }
    message(str_c("run_object | batch: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}