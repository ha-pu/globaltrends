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
#' compute_score(control = 1, object = 1, locations = countries)
#' compute_score(control = 1, object = as.list(1:5), locations = countries)
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

compute_score <- function(control, object, locations = countries) UseMethod("compute_score", object)

#' @rdname compute_score
#' @method compute_score numeric
#' @export

compute_score.numeric <- function(control, object, locations = countries) {
  control <- control[[1]]
  walk(c(control, object), .test_batch)
  walk(locations, ~ {
    if (.test_empty(table = "data_score", batch_c = control, batch_o = object, location = .x)) {
      qry_mapping <- filter(data_mapping, batch_c == control & batch_o == object & location == .x)
      qry_mapping <- collect(qry_mapping)
      if (nrow(qry_mapping) != 0) {
        qry_control <- filter(data_control, batch == control & location == .x)
        qry_control <- collect(qry_control)
        qry_object <- filter(data_object, batch == object & location == .x)
        qry_object <- collect(qry_object)

        qry_control <- mutate(qry_control, date = as_date(date))
        qry_object <- mutate(qry_object, date = as_date(date))
        qry_mapping <- mutate(qry_mapping, date = as_date(date))

        # adapt time series frequency
        qry_control <- .reset_date(qry_control)
        qry_object <- .reset_date(qry_object)
        qry_mapping <- .reset_date(qry_mapping)

        if (min(nrow(count(qry_control, date)), nrow(count(qry_object, date)), nrow(count(qry_mapping, date))) >= 24) {
          # adjust to time series and impute negative values
          qry_control <- nest(qry_control, data = c(date, hits))
          qry_control <- mutate(qry_control, data = map(data, .adjust_ts))
          qry_control <- unnest(qry_control, data)
          qry_control <- mutate(qry_control,
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
          qry_object <- nest(qry_object, data = c(date, hits))
          qry_object <- mutate(qry_object, data = map(data, .adjust_ts))
          qry_object <- unnest(qry_object, data)
          qry_object <- mutate(qry_object,
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
          qry_mapping <- nest(qry_mapping, data = c(date, hits))
          qry_mapping <- mutate(qry_mapping, data = map(data, .adjust_ts))
          qry_mapping <- unnest(qry_mapping, data)
          qry_mapping <- mutate(qry_mapping,
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
        qry_control <- pivot_longer(qry_control, cols = contains("hits"), names_to = "key", values_to = "value")
        qry_object <- pivot_longer(qry_object, cols = contains("hits"), names_to = "key", values_to = "value")
        qry_mapping <- pivot_longer(qry_mapping, cols = contains("hits"), names_to = "key", values_to = "value")

        # set to benchmark
        tmp_con <- inner_join(qry_mapping, qry_control, by = c("location", "keyword", "date", "key"), suffix = c("_m", "_c"))
        tmp_con <- mutate(tmp_con,
          value_m = case_when(value_m == 0 ~ 1, TRUE ~ value_m),
          value_c = case_when(value_c == 0 ~ 1, TRUE ~ value_c)
        )
        tmp_con <- mutate(tmp_con, benchmark = coalesce(value_m / value_c, 0))
        tmp_con <- select(tmp_con, location, date, key, benchmark)
        tmp_con <- inner_join(tmp_con, qry_control, by = c("location", "date", "key"))
        tmp_con <- mutate(tmp_con, value = value * benchmark)
        tmp_con <- select(tmp_con, location, date, key, keyword, value)

        tmp_obj <- inner_join(qry_mapping, qry_object, by = c("location", "keyword", "date", "key"), suffix = c("_m", "_o"))
        tmp_obj <- mutate(tmp_obj,
          value_m = case_when(value_m == 0 ~ 1, TRUE ~ value_m),
          value_o = case_when(value_o == 0 ~ 1, TRUE ~ value_o)
        )
        tmp_obj <- mutate(tmp_obj, benchmark = coalesce(value_m / value_o, 0))
        tmp_obj <- select(tmp_obj, location, date, key, benchmark)
        tmp_obj <- inner_join(tmp_obj, qry_object, by = c("location", "date", "key"))
        tmp_obj <- mutate(tmp_obj, value = value * benchmark)
        tmp_obj <- select(tmp_obj, location, date, key, keyword, value)

        # compute score
        control_agg <- group_by(tmp_con, location, date, key)
        control_agg <- summarise(control_agg, value_c = sum(value))
        control_agg <- ungroup(control_agg)
        object_agg <- left_join(tmp_obj, control_agg, by = c("location", "date", "key"))
        object_agg <- mutate(object_agg,
          score = coalesce(value / value_c, 0),
          key = str_replace(key, "hits_", "score_")
        )
        object_agg <- select(object_agg, location, date, keyword, key, score)
        data_score <- pivot_wider(object_agg, names_from = key, values_from = score, values_fill = 0)
        out <- mutate(data_score, batch_c = control, batch_o = object)
        dbWriteTable(conn = globaltrends_db, name = "data_score", value = out, append = TRUE)
      }
    }
    message(glue("Successfully computed search score | control: {control} | object: {object} | location: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
  })
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(control, object, locations = countries) {
  walk(object, compute_score, control = control, locations = locations)
}
