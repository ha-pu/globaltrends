# aggregate country score data

run_agg <- function (control, object) {
  
  if (.test_empty(table = "data_agg", batch_c = control, batch_o = object)){
    data <- dpylr::collect(filter(data_score, batch_c == control & batch_o == object))
    
    # run dict replace
    if (any(data$keyword %in% dict_obj$term1)) {
      kw1 <- unique(data$keyword[data$keyword %in% dict_obj$term1])
      out <- purrr::map_dfr(kw1, ~{
        kw2 <- dict_obj$term2[dict_obj$term1 == .x]
        if (!any(kw2 %in% data$keyword)) {
          out <- terms_obj$batch[terms_obj$keyword == kw2]
	  out <- dplyr::filter(data_score, batch_c == control & batch_o == out)
          out <- dplyr::collect(out)
          out <- out[out$keyword == kw2,]
          return(out)
        }
      })
      data <- dplyr::bind_rows(data, out)
      data$keyword <- stringr::str_replace_all(data$keyword, purrr::set_names(dict_obj$term1[dict_obj$term1 %in% kw1], dict_obj$term2[dict_obj$term1 %in% kw1]))
      data <- data %>%
        dplyr::group_by(geo, date, keyword, batch_c, batch_o) %>%
        dplyr::summarise_if(is.double, sum) %>%
        dplyr::ungroup()
    }
    data <- data[!(data$keyword %in% dict_obj$term2),]
    
    # compute doi measures
    out <- data %>%
      tidyr::gather(key = "type", value = "score", contains("score")) %>%
      tidyr::nest(geo, score) %>%
      dplyr::mutate(gini = purrr::map_dbl(data, ~ .compute_gini(series = .x$score)),
             hhi = purrr::map_dbl(data, ~ .compute_hhi(series = .x$score)),
             entropy = purrr::map_dbl(data, ~ .compute_entropy(series = .x$score))) %>%
      dplyr::select(-data)
    
    # write data
    out <- dplyr::mutate(out, batch_c = control, batch_o = object)
    DBI::dbWriteTable(conn = gtrends_db, name = "data_agg", value = out, append = TRUE)
  }
  message(stringr::str_c("run_agg | control: ", control, " | object: ", object, " complete [", object, "|", max(terms_obj$batch), "]"))
}
