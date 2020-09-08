add_batch <- function(type, keyword, time = "2010-01-01 2020-07-31") {
  if (type == "control") {
    new_batch <- max(terms_con$batch) + 1
    data <- tibble(batch = new_batch, keyword, type = "con")
    dbWriteTable(conn = gtrends_db, name = "batch_terms", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "con")
    dbWriteTable(conn = gtrends_db, name = "batch_time", value = data, append = TRUE)
    terms_con <- filter(batch_terms, type == "con")
    terms_con <<- collect(terms_con)
    time_con <- filter(batch_time, type == "con")
    time_con <<- collect(time_con)
    message(str_c("New control batch", new_batch, "created.", sep = " "))
  } else if (type == "object") {
    new_batch <- max(terms_obj$batch) + 1
    data <- tibble(batch = new_batch, keyword, type = "obj")
    dbWriteTable(conn = gtrends_db, name = "batch_terms", value = data, append = TRUE)
    data <- tibble(batch = new_batch, time = time, type = "obj")
    dbWriteTable(conn = gtrends_db, name = "batch_time", value = data, append = TRUE)
    terms_obj <- filter(batch_terms, type == "obj")
    terms_obj <<- collect(terms_obj)
    time_obj  <- filter(batch_time, type == "obj")
    time_obj <<- collect(time_obj)
    message(str_c("New object batch", new_batch, "created.", sep = " "))
  }
}
