#' @title
#' @description
#' @details
#' @seealso
#' @return
#' @example
#' @export
#' @importFrom

run_score <- function(control, object, lst_geo = lst_wdi) {
  purrr::map(lst_geo, ~ {
    if (.test_empty(table = "data_score", batch_c = control, batch_o = object, geo = .x)) {
      qry_map <- dplyr::filter(data_map, batch_c == control & batch_o == object & geo == .x)
      qry_map <- dplyr::collect(qry_map)
      if (nrow(qry_map) != 0) {
        qry_con <- dplyr::filter(data_con, batch == control & geo == .x)
        qry_con <- dplyr::collect(qry_con)
        qry_obj <- dplyr::filter(data_obj, batch == object & geo == .x)
        qry_obj <- dplyr::collect(qry_obj)

        qry_con <- dplyr::mutate(qry_con, date = lubridate::as_date(date))
        qry_obj <- dplyr::mutate(qry_obj, date = lubridate::as_date(date))
        qry_map <- dplyr::mutate(qry_map, date = lubridate::as_date(date))

        # test for time series
        qry_con <- .reset_date(qry_con)
        qry_obj <- .reset_date(qry_obj)
        qry_map <- .reset_date(qry_map)

        if (min(nrow(dplyr::count(qry_con, date)), nrow(dplyr::count(qry_obj, date)), nrow(dplyr::count(qry_map, date))) >= 24) {
          # adjust to time series and impute negative values
          qry_con <- qry_con %>%
            tidyr::nest(date, hits) %>%
            dplyr::mutate(data = purrr::map(data, .adjust_ts)) %>%
            tidyr::unnest(data) %>%
            dplyr::mutate(
              hits_trd = dplyr::case_when(
                hits_trd < 0 & hits_sad < 0 ~ 0.1,
                hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                TRUE ~ hits_trd
              ),
              hits_sad = dplyr::case_when(
                hits_sad < 0 & hits_trd < 0 ~ 0.1,
                hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                TRUE ~ hits_sad
              )
            )
          qry_obj <- qry_obj %>%
            tidyr::nest(date, hits) %>%
            dplyr::mutate(data = purrr::map(data, .adjust_ts)) %>%
            tidyr::unnest(data) %>%
            dplyr::mutate(
              hits_trd = dplyr::case_when(
                hits_trd < 0 & hits_sad < 0 ~ 0.1,
                hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                TRUE ~ hits_trd
              ),
              hits_sad = dplyr::case_when(
                hits_sad < 0 & hits_trd < 0 ~ 0.1,
                hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                TRUE ~ hits_sad
              )
            )
          qry_map <- qry_map %>%
            tidyr::nest(date, hits) %>%
            dplyr::mutate(data = purrr::map(data, .adjust_ts)) %>%
            tidyr::unnest(data) %>%
            dplyr::mutate(
              hits_trd = dplyr::case_when(
                hits_trd < 0 & hits_sad < 0 ~ 0,
                hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                TRUE ~ hits_trd
              ),
              hits_sad = dplyr::case_when(
                hits_sad < 0 & hits_trd < 0 ~ 0,
                hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                TRUE ~ hits_sad
              )
            )
        }
        qry_con <- tidyr::gather(qry_con, "key", "value", dplyr::contains("hits"))
        qry_obj <- tidyr::gather(qry_obj, "key", "value", dplyr::contains("hits"))
        qry_map <- tidyr::gather(qry_map, "key", "value", dplyr::contains("hits"))

        # set to benchmark
        qry_con <- qry_map %>%
          dplyr::inner_join(qry_con, by = c("geo", "keyword", "date", "key"), suffix = c("_m", "_c")) %>%
          dplyr::mutate(
            value_m = dplyr::case_when(value_m == 0 ~ 1, TRUE ~ value_m),
            value_c = dplyr::case_when(value_c == 0 ~ 1, TRUE ~ value_c)
          ) %>%
          dplyr::mutate(benchmark = dplyr::coalesce(value_m / value_c, 0)) %>%
          dplyr::select(geo, date, key, benchmark) %>%
          dplyr::inner_join(qry_con, by = c("geo", "date", "key")) %>%
          dplyr::mutate(value = value * benchmark) %>%
          dplyr::select(-benchmark)

        qry_obj <- qry_map %>%
          dplyr::inner_join(qry_obj, by = c("geo", "keyword", "date", "key"), suffix = c("_m", "_o")) %>%
          dplyr::mutate(
            value_m = dplyr::case_when(value_m == 0 ~ 1, TRUE ~ value_m),
            value_o = dplyr::case_when(value_o == 0 ~ 1, TRUE ~ value_o)
          ) %>%
          dplyr::mutate(benchmark = dplyr::coalesce(value_m / value_o, 0)) %>%
          dplyr::select(geo, date, key, benchmark) %>%
          dplyr::inner_join(qry_obj, by = c("geo", "date", "key")) %>%
          dplyr::mutate(value = value * benchmark) %>%
          dplyr::select(-benchmark)

        # compute score
        data_con_agg <- qry_con %>%
          dplyr::group_by(geo, date, key) %>%
          dplyr::summarise(value_c = sum(value)) %>%
          dplyr::ungroup()
        data_obj_agg <- qry_obj %>%
          dplyr::left_join(data_con_agg, by = c("geo", "date", "key")) %>%
          dplyr::mutate(
            score = dplyr::coalesce(value / value_c, 0),
            key = stringr::str_replace(key, "hits_", "score_")
          ) %>%
          dplyr::select(geo, date, keyword, key, score)
        data_score <- tidyr::spread(data_obj_agg, key, score, fill = 0)
        out <- dplyr::mutate(data_score, batch_c = control, batch_o = object)
        DBI::dbWriteTable(conn = gtrends_db, name = "data_score", value = out, append = TRUE)
      }
    }
    message(stringr::str_c("run_score | control: ", control, " | object: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
