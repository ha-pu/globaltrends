run_control <- function(control, lst_geo = lst_wdi) {
  terms <- terms_con$keyword[terms_con$batch == control]
  time <- time_con$time[time_con$batch == control]
  map(lst_geo, ~{
    if (.test_empty(table = "data_con", batch_c = control, geo = .x)){
      out <- .get_trend(geo = .x, term = terms, time = time)
      if (!is.null(out)) {
        out <- mutate(out, batch = control)
        dbWriteTable(conn = gtrends_db, name = "data_con", value = out, append = TRUE)
      }
    }
    message(str_c("run_control | control: ", control, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
