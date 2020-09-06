run_score <- function(control, object, lst_geo = lst_wdi) {
  source("code/hlpr_adjust_ts.r")
  source("code/hlpr_reset_date.r")
  
  if (packageVersion("tidyr") >= "1.0.0") {
    nest <- nest_legacy
	  unnest <- unnest_legacy
  }
  
  map(lst_geo, ~{
    if (.test_empty(table = "data_score", batch_c = control, batch_o = object, geo = .x)){
      qry_map <- collect(filter(data_map, batch_c == control & batch_o == object & geo == .x))
      if(nrow(qry_map) != 0){
        qry_con <- collect(filter(data_con, batch == control & geo == .x))
        qry_obj <- collect(filter(data_obj, batch == object & geo == .x))
        
        qry_con <- mutate(qry_con, date = as_date(date))
        qry_obj <- mutate(qry_obj, date = as_date(date))
        qry_map <- mutate(qry_map, date = as_date(date))
        
        # test for time series
        qry_con <- .reset_date(qry_con)
        qry_obj <- .reset_date(qry_obj)
        qry_map <- .reset_date(qry_map)
        
        if (min(nrow(count(qry_con, date)), nrow(count(qry_obj, date)), nrow(count(qry_map, date))) >= 24) {
          # adjust to time series and impute negative values
          qry_con <- qry_con %>%
            nest(date, hits) %>%
            mutate(data = map(data, .adjust_ts)) %>%
            unnest(data) %>%
            mutate(hits_trd = case_when(hits_trd < 0 & hits_sad < 0 ~ 0.1,
                                        hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                                        TRUE ~ hits_trd),
                   hits_sad = case_when(hits_sad < 0 & hits_trd < 0 ~ 0.1,
                                        hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                                        TRUE ~ hits_sad)
            )
          qry_obj <- qry_obj %>%
            nest(date, hits) %>%
            mutate(data = map(data, .adjust_ts)) %>%
            unnest(data) %>%
            mutate(hits_trd = case_when(hits_trd < 0 & hits_sad < 0 ~ 0.1,
                                        hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                                        TRUE ~ hits_trd),
                   hits_sad = case_when(hits_sad < 0 & hits_trd < 0 ~ 0.1,
                                        hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                                        TRUE ~ hits_sad)
            )
          qry_map <- qry_map %>%
            nest(date, hits) %>%
            mutate(data = map(data, .adjust_ts)) %>%
            unnest(data) %>%
            mutate(hits_trd = case_when(hits_trd < 0 & hits_sad < 0 ~ 0,
                                        hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                                        TRUE ~ hits_trd),
                   hits_sad = case_when(hits_sad < 0 & hits_trd < 0 ~ 0,
                                        hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                                        TRUE ~ hits_sad)
            )
        }
        qry_con <- gather(qry_con, "key", "value", contains("hits"))
        qry_obj <- gather(qry_obj, "key", "value", contains("hits"))
        qry_map <- gather(qry_map, "key", "value", contains("hits"))
        
        # set to benchmark
        qry_con <- qry_map %>%
          inner_join(qry_con, by = c("geo", "keyword", "date", "key"), suffix = c("_m", "_c")) %>%
          mutate(value_m = case_when(value_m == 0 ~ 1, TRUE ~ value_m),
                 value_c = case_when(value_c == 0 ~ 1, TRUE ~ value_c)) %>%
          mutate(benchmark = coalesce(value_m / value_c, 0)) %>%
          select(geo, date, key, benchmark) %>%
          inner_join(qry_con, by = c("geo", "date", "key")) %>%
          mutate(value = value * benchmark) %>%
          select(-benchmark)
        
        qry_obj <- qry_map %>%
          inner_join(qry_obj, by = c("geo", "keyword", "date", "key"), suffix = c("_m", "_o")) %>%
          mutate(value_m = case_when(value_m == 0 ~ 1, TRUE ~ value_m),
                 value_o = case_when(value_o == 0 ~ 1, TRUE ~ value_o)) %>%
          mutate(benchmark = coalesce(value_m / value_o, 0)) %>%
          select(geo, date, key, benchmark) %>%
          inner_join(qry_obj, by = c("geo", "date", "key")) %>%
          mutate(value = value * benchmark) %>%
          select(-benchmark)
        
        # compute score
        data_con_agg <- qry_con %>%
          group_by(geo, date, key) %>%
          summarise(value_c = sum(value)) %>%
          ungroup()
        data_obj_agg <- qry_obj %>%
          left_join(data_con_agg, by = c("geo", "date", "key")) %>%
          mutate(score = coalesce(value / value_c, 0),
                 key = str_replace(key, "hits_", "score_")) %>%
          select(geo, date, keyword, key, score)
        data_score <- spread(data_obj_agg, key, score, fill = 0)
        out <- mutate(data_score, batch_c = control, batch_o = object)
        dbWriteTable(conn = gtrends_db, name = "data_score", value = out, append = TRUE)
      }
    }
    message(str_c("run_score | control: ", control, " | object: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
