run_agg <- function (control, object) {
  source("code/hlpr_compute_doi.r")
  
  if (packageVersion("tidyr") >= "1.0.0") {
    nest <- nest_legacy
	unnest <- unnest_legacy
  }
  
  if (.test_empty(table = "data_agg", batch_c = control, batch_o = object)){
    data <- collect(filter(data_score, batch_c == control & batch_o == object))
    
    # run dict replace
    if (any(data$keyword %in% dict_obj$term1)) {
      kw1 <- unique(data$keyword[data$keyword %in% dict_obj$term1])
      out <- map_dfr(kw1, ~{
        kw2 <- dict_obj$term2[dict_obj$term1 == .x]
        if (!any(kw2 %in% data$keyword)) {
          out <- terms_obj$batch[terms_obj$keyword == kw2]
          out <- collect(filter(data_score, batch_c == control & batch_o == out))
          out <- out[out$keyword == kw2,]
          return(out)
        }
      })
      data <- bind_rows(data, out)
      data$keyword <- str_replace_all(data$keyword, set_names(dict_obj$term1[dict_obj$term1 %in% kw1], dict_obj$term2[dict_obj$term1 %in% kw1]))
      data <- data %>%
        group_by(geo, date, keyword, batch_c, batch_o) %>%
        summarise_if(is.double, sum) %>%
        ungroup()
    }
    data <- data[!(data$keyword %in% dict_obj$term2),]
    
    # compute doi measures
    out <- data %>%
      gather(key = "type", value = "score", contains("score")) %>%
      nest(geo, score) %>%
      mutate(gini = map_dbl(data, ~ .compute_gini(series = .x$score)),
             hhi = map_dbl(data, ~ .compute_hhi(series = .x$score)),
             entropy = map_dbl(data, ~ .compute_entropy(series = .x$score))) %>%
      select(-data)
    
    # write data
    out <- mutate(out, batch_c = control, batch_o = object)
    dbWriteTable(conn = gtrends_db, name = "data_agg", value = out, append = TRUE)
  }
  message(str_c("run_agg | control: ", control, " | object: ", object, " complete [", object, "|", max(terms_obj$batch), "]"))
}
