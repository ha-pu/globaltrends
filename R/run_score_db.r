#' @title Compute keyword-country search score
#'
#' @description
#' @details
#'
#' @param control Control batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param lst_geo List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{gtrends_base}.
#'
#' @seealso
#'
#' @return Message that data was computed successfully. Data is uploaded
#' to data_score.
#'
#' @examples
#' \dontrun{
#' data_score(control = 1, object = 1, lst_geo = lst_wdi)
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr case_when
#' @importFrom dplyr coalesce
#' @importFrom dplyr collect
#' @importFrom dplyr contains
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom lubridate as_date
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom stringr str_replace
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom tidyr spread
#' @importFrom tidyr unnest

run_score <- function(control, object, lst_geo = lst_wdi) {
  x <- map(lst_geo, ~ {
    if (.test_empty(table = "data_score", batch_c = control, batch_o = object, geo = .x)) {
      qry_map <- filter(data_map, batch_c == control & batch_o == object & geo == .x)
      qry_map <- collect(qry_map)
      if (nrow(qry_map) != 0) {
        qry_con <- filter(data_con, batch == control & geo == .x)
        qry_con <- collect(qry_con)
        qry_obj <- filter(data_obj, batch == object & geo == .x)
        qry_obj <- collect(qry_obj)

        qry_con <- mutate(qry_con, date = as_date(date))
        qry_obj <- mutate(qry_obj, date = as_date(date))
        qry_map <- mutate(qry_map, date = as_date(date))

        # test for time series
        qry_con <- .reset_date(qry_con)
        qry_obj <- .reset_date(qry_obj)
        qry_map <- .reset_date(qry_map)

        if (min(nrow(count(qry_con, date)), nrow(count(qry_obj, date)), nrow(count(qry_map, date))) >= 24) {
          # adjust to time series and impute negative values
          qry_con <- nest(qry_con, data = c(date, hits))
          qry_con <- mutate(qry_con, data = map(data, .adjust_ts))
          qry_con <- unnest(qry_con, data)
          qry_con <- mutate(qry_con,
              hits_trd = case_when(
                hits_trd < 0 & hits_sad < 0 ~ 0.1,
                hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                TRUE ~ hits_trd
              ),
              hits_sad = case_when(
                hits_sad < 0 & hits_trd < 0 ~ 0.1,
                hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                TRUE ~ hits_sad
              )
            )
	  qry_obj <- nest(qry_obj, data = c(date, hits))
          qry_obj <- mutate(qry_obj, data = map(data, .adjust_ts))
          qry_obj <- unnest(qry_obj, data)
          qry_obj <- mutate(qry_obj,
              hits_trd = case_when(
                hits_trd < 0 & hits_sad < 0 ~ 0.1,
                hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                TRUE ~ hits_trd
              ),
              hits_sad = case_when(
                hits_sad < 0 & hits_trd < 0 ~ 0.1,
                hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                TRUE ~ hits_sad
              )
            )
          qry_map <- nest(qry_map, data = c(date, hits))
          qry_map <- mutate(qry_map, data = map(data, .adjust_ts))
          qry_map <- unnest(qry_map, data)
          qry_map <- mutate(qry_map,
              hits_trd = case_when(
                hits_trd < 0 & hits_sad < 0 ~ 0.1,
                hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
                TRUE ~ hits_trd
              ),
              hits_sad = case_when(
                hits_sad < 0 & hits_trd < 0 ~ 0.1,
                hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
                TRUE ~ hits_sad
              )
            )
        }
        qry_con <- gather(qry_con, "key", "value", contains("hits"))
        qry_obj <- gather(qry_obj, "key", "value", contains("hits"))
        qry_map <- gather(qry_map, "key", "value", contains("hits"))

        # set to benchmark
        tmp_con <- inner_join(qry_map, qry_con, by = c("geo", "keyword", "date", "key"), suffix = c("_m", "_c"))
        tmp_con <- mutate(tmp_con,
            value_m = case_when(value_m == 0 ~ 1, TRUE ~ value_m),
            value_c = case_when(value_c == 0 ~ 1, TRUE ~ value_c)
          )
        tmp_con <- mutate(tmp_con, benchmark = coalesce(value_m / value_c, 0))
        tmp_con <- select(tmp_con, geo, date, key, benchmark)
        tmp_con <- inner_join(tmp_con, qry_con, by = c("geo", "date", "key"))
        tmp_con <- mutate(tmp_con, value = value * benchmark)
        tmp_con <- select(tmp_con, geo, date, key, keyword, value)

        tmp_obj <- inner_join(qry_map, qry_obj, by = c("geo", "keyword", "date", "key"), suffix = c("_m", "_o"))
        tmp_obj <- mutate(tmp_obj,
            value_m = case_when(value_m == 0 ~ 1, TRUE ~ value_m),
            value_o = case_when(value_o == 0 ~ 1, TRUE ~ value_o)
          )
        tmp_obj <- mutate(tmp_obj, benchmark = coalesce(value_m / value_o, 0))
        tmp_obj <- select(tmp_obj, geo, date, key, benchmark)
        tmp_obj <- inner_join(tmp_obj, qry_obj, by = c("geo", "date", "key"))
        tmp_obj <- mutate(tmp_obj, value = value * benchmark)
        tmp_obj <- select(tmp_obj, geo, date, key, keyword, value)

        # compute score
        data_con_agg <- group_by(tmp_con, geo, date, key)
        data_con_agg <- summarise(data_con_agg, value_c = sum(value))
        data_con_agg <- ungroup(data_con_agg)
        data_obj_agg <- left_join(tmp_obj, data_con_agg, by = c("geo", "date", "key"))
        data_obj_agg <- mutate(data_obj_agg,
            score = coalesce(value / value_c, 0),
            key = str_replace(key, "hits_", "score_")
          )
        data_obj_agg <- select(data_obj_agg, geo, date, keyword, key, score)
        data_score <- spread(data_obj_agg, key, score, fill = 0)
        out <- mutate(data_score, batch_c = control, batch_o = object)
        dbWriteTable(conn = gtrends_db, name = "data_score", value = out, append = TRUE)
      }
    }
    message(str_c("run_score | control: ", control, " | object: ", object, " | geo: ", .x, " complete [", which(lst_geo == .x), "|", length(lst_geo), "]"))
  })
}
