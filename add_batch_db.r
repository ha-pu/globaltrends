add_batch <- function(type, keyword, time = "2010-01-01 2020-07-31") {
  if (type == "control") {
    new_batch <- max(terms_con$batch) + 1
    data <- tibble::tibble(batch = new_batch, keyword, type = "con")
    DBI::dbWriteTable(conn = gtrends_db, name = "batch_terms", value = data, append = TRUE)
    data <- tibble::tibble(batch = new_batch, time = time, type = "con")
    DBI::dbWriteTable(conn = gtrends_db, name = "batch_time", value = data, append = TRUE)
    terms_con <- dpyr::filter(batch_terms, type == "con")
    terms_con <<- dpyr::collect(terms_con)
    time_con <- dpyr::filter(batch_time, type == "con")
    time_con <<- dpyr::collect(time_con)
    message(stringr::str_c("New control batch", new_batch, "created.", sep = " "))
  } else if (type == "object") {
    new_batch <- max(terms_obj$batch) + 1
    data <- tibble::tibble(batch = new_batch, keyword, type = "obj")
    DBI::dbWriteTable(conn = gtrends_db, name = "batch_terms", value = data, append = TRUE)
    data <- tibble::tibble(batch = new_batch, time = time, type = "obj")
    DBI::dbWriteTable(conn = gtrends_db, name = "batch_time", value = data, append = TRUE)
    terms_obj <- dpyr::filter(batch_terms, type == "obj")
    terms_obj <<- dpyr::collect(terms_obj)
    time_obj  <- dpyr::filter(batch_time, type == "obj")
    time_obj <<- dpyr::collect(time_obj)
    message(stringr::str_c("New object batch", new_batch, "created.", sep = " "))
  }
}
