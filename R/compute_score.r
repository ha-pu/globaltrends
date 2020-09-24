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
#' of class \code{numeric}. Defaults to 1.
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric} or object of class \code{list} containing single
#' objects of class \code{numeric}.
#' @param locations List of countries or regions for which the data is
#' downloaded. Refers to lists generated in \code{start_db}.
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
#' @importFrom dplyr anti_join
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

compute_score <- function(control = 1, object, locations = countries) UseMethod("compute_score", object)

#' @rdname compute_score
#' @method compute_score numeric
#' @export

compute_score.numeric <- function(control = 1, object, locations = countries) {
  if (length(control) > 1) stop(glue("Error: 'control' must be object of length 1.\nYou provided an object of length {length(control)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be object of type character.\nYou provided an object of type {typeof(locations)}."))
  if (length(object) > 1) {
    compute_score(control = control, object = as.list(object), locations = locations)
  } else {
    control <- control[[1]]
    walk(c(control, object), .test_batch)
    walk(locations, ~ {
      if (.test_empty(
        table = "data_score",
        batch_c = control,
        batch_o = object,
        location = .x
      )) {
        qry_object <- filter(
          .tbl_object,
          batch_c == control & batch_o == object & location == .x
        )
        qry_object <- collect(qry_object)
        if (nrow(qry_object) != 0) {
          qry_control <- filter(.tbl_control, batch == control & location == .x)
          qry_control <- collect(qry_control)

          qry_control <- mutate(qry_control, date = as_date(date))
          qry_object <- mutate(qry_object, date = as_date(date))

          # adapt time series frequency
          qry_control <- .reset_date(qry_control)
          qry_object <- .reset_date(qry_object)

          if (
            min(
              nrow(count(qry_control, date)),
              nrow(count(qry_object, date))
            ) >= 24
          ) {
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
          }
          qry_control <- pivot_longer(
            qry_control,
            cols = contains("hits"),
            names_to = "key",
            values_to = "value"
          )
          qry_object <- pivot_longer(
            qry_object,
            cols = contains("hits"),
            names_to = "key",
            values_to = "value"
          )

          # set to benchmark
          data_control <- inner_join(qry_object,
            qry_control,
            by = c(
              "location",
              "keyword",
              "date",
              "key"
            ),
            suffix = c("_o", "_c")
          )
          data_control <- mutate(data_control,
            value_o = case_when(
              value_o == 0 ~ 1,
              TRUE ~ value_o
            ),
            value_c = case_when(
              value_c == 0 ~ 1,
              TRUE ~ value_c
            )
          )
          data_control <- mutate(data_control,
            benchmark = coalesce(value_o / value_c, 0)
          )
          data_control <- select(data_control, location, date, key, benchmark)
          data_control <- inner_join(
            data_control,
            qry_control,
            by = c("location", "date", "key")
          )
          data_control <- mutate(data_control, value = value * benchmark)
          data_control <- select(
            data_control,
            location,
            date,
            key,
            keyword,
            value
          )

          data_object <- anti_join(qry_object, data_control, by = c("keyword"))

          # compute score
          data_control <- group_by(data_control, location, date, key)
          data_control <- summarise(data_control, value_c = sum(value), .groups = "drop")
          data_object <- left_join(
            data_object,
            data_control,
            by = c("location", "date", "key")
          )
          data_object <- mutate(data_object,
            score = coalesce(value / value_c, 0),
            key = str_replace(key, "hits_", "score_")
          )
          data_object <- select(data_object, location, date, keyword, key, score)
          out <- pivot_wider(
            data_object,
            names_from = key,
            values_from = score,
            values_fill = 0
          )
          out <- mutate(
            out,
            batch_c = control,
            batch_o = object,
            synonym = case_when(
              keyword %in% .keyword_synonyms$synonym ~ TRUE,
              TRUE ~ FALSE
            )
          )
          dbWriteTable(
            conn = globaltrends_db,
            name = "data_score",
            value = out,
            append = TRUE
          )
        }
      }
      message(glue("Successfully computed search score | control: {control} | object: {object} | location: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
    })
    .aggregate_synonym()
  }
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(control = 1, object, locations = countries) {
  walk(object, compute_score, control = control, locations = locations)
}

#' @rdname compute_score
#' @export

compute_score_global <- function(control = 1, object) {
  compute_score(control = control, object = object, locations = "world")
}
