#' @title Compute keyword-country search score
#'
#' @aliases
#' compute_score
#' compute_score.numeric
#' compute_score.list
#'
#' @description
#' @details
#'
#' @param control Control batch for which the data is downloaded. Object
#' of class \code{numeric}.
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' elements of class \code{numeric}.
#' @param locations List of countries or regions for which the data is downloaded.
#' Refers to lists generated in \code{start_db}.
#'
#' @seealso
#'
#' @return Message that data was computed successfully. Data is uploaded
#' to data_score.
#'
#' @examples
#' \dontrun{
#' data_score(control = 1, object = 1, locations = lst_wdi)
#' data_score(control = 1, object = as.list(1:5), locations = lst_wdi)
#' }
#'
#' @export
#' @rdname compute_score
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
#' @importFrom glue glue
#' @importFrom lubridate as_date
#' @importFrom purrr walk
#' @importFrom stringr str_replace
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest

compute_score <- function(control, object, locations = lst_wdi) UseMethod("compute_score", object)

#' @rdname compute_score
#' @method compute_score numeric
#' @export

compute_score.numeric <- function(control, object, locations = lst_wdi) {
  control <- control[[1]]
  walk(c(control, object), .test_batch)
  walk(locations, ~ {
    if (.test_empty(table = "data_score", batch_c = control, batch_o = object, geo = .x)) {
      qry_map <- filter(data_mapping, batch_c == control & batch_o == object & geo == .x)
      qry_map <- collect(qry_map)
      if (nrow(qry_map) != 0) {
        qry_con <- filter(data_control, batch == control & geo == .x)
        qry_con <- collect(qry_con)
        qry_obj <- filter(data_object, batch == object & geo == .x)
        qry_obj <- collect(qry_obj)

        qry_con <- mutate(qry_con, date = as_date(date))
        qry_obj <- mutate(qry_obj, date = as_date(date))
        qry_map <- mutate(qry_map, date = as_date(date))

        # adapt time series frequency
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
        qry_con <- pivot_longer(qry_con, cols = contains("hits"), names_to = "key", values_to = "value")
        qry_obj <- pivot_longer(qry_obj, cols = contains("hits"), names_to = "key", values_to = "value")
        qry_map <- pivot_longer(qry_map, cols = contains("hits"), names_to = "key", values_to = "value")

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
        control_agg <- group_by(tmp_con, geo, date, key)
        control_agg <- summarise(control_agg, value_c = sum(value))
        control_agg <- ungroup(control_agg)
        object_agg <- left_join(tmp_obj, control_agg, by = c("geo", "date", "key"))
        object_agg <- mutate(object_agg,
          score = coalesce(value / value_c, 0),
          key = str_replace(key, "hits_", "score_")
        )
        object_agg <- select(object_agg, geo, date, keyword, key, score)
        data_score <- pivot_wider(object_agg, names_from = key, values_from = score, values_fill = 0)
        out <- mutate(data_score, batch_c = control, batch_o = object)
        dbWriteTable(conn = doiGT_DB, name = "data_score", value = out, append = TRUE)
      }
    }
    message(glue("Successfully computed search score | control: {control} | object: {object} | geo: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
  })
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(control, object, locations = lst_wdi) {
  walk(object, compute_score, control = control, locations = locations)
}