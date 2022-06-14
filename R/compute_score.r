#' @title Compute keyword-country search score
#'
#' @aliases
#' compute_score
#' compute_score.numeric
#' compute_score.list
#'
#' @description
#' The function computes search scores for object keywords. Search volumes for
#' control and object batches are mapped to the same base. Next, search volumes
#' for object batches are divided by the sum of search volumes for the
#' respective control batch. `compute_voi` computes volume of
#' internationalization (VOI) as global search scores.
#'
#' @details
#' The search score computation proceeds in four steps. First, the function
#' aggregates all search volumes to monthly data. Then, it applies some optional
#' time series adjustments: seasonally adjusted [`forecast::seasadj`] and
#' trend only [`stats::stl`]. Next, it follows the procedure outlined by
#' Castelnuovo and Tran (2017, pp. A1-A2) to map control and object data. After
#' the mapping, object search volumes are divided by the sum of control search
#' volumes in the respective control batch. We use the sum of search volumes for
#' a set of control keywords, rather than the search volumes for a single
#' control keyword, to smooth-out variation in the underlying control data. When
#' synonyms were specified through `add_synonym`, search scores for
#' synonyms are added to the main keyword.
#'
#' *Castelnuovo, E. & Tran, T. D. 2017. Google It Up! A Google Trends-based
#' Uncertainty index for the United States and Australia. Economics Letters,
#' 161: 149-153.*
#'
#'
#' @param control Control batch for which the data is downloaded. Object
#' of type `numeric`. Defaults to 1.
#' @param object Object batch for which the data is downloaded. Object
#' of type `numeric` or object of type `list` containing single
#' objects of type `numeric`.
#' @param locations List of countries or regions for which the data is
#' downloaded. Refers to lists generated in `start_db`. Defaults to
#' `countries`.
#'
#' @seealso
#' * [example_score()]
#' * [add_synonym()]
#' * [stats::stl()]
#' * [forecast::seasadj()]
#'
#' @return Message that data has been computed successfully. Data is written to
#' table *data_score*.
#'
#' @examples
#' \dontrun{
#' compute_score(
#'   object = 1,
#'   control = 1,
#'   locations = countries
#' )
#' compute_voi(
#'   object = 1,
#'   control = 1
#' )
#' compute_score(
#'   object = as.list(1:5),
#'   control = 1,
#'   locations = countries
#' )
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
#' @importFrom rlang .data
#' @importFrom rlang env_parent
#' @importFrom stringr str_replace
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest

compute_score <- function(object, control = 1, locations = gt.env$countries) UseMethod("compute_score", object)

#' @rdname compute_score
#' @method compute_score numeric
#' @export

compute_score.numeric <- function(object, control = 1, locations = gt.env$countries) {
  control <- unlist(control)
  .check_length(control, 1)
  .check_input(locations, "character")
  if (length(object) > 1) {
    compute_score(control = control, object = as.list(object), locations = locations)
  } else {
    walk(list(control, object), .check_batch)
    ts_control <- TRUE
    ts_object <- TRUE
    walk(locations, ~ {
      if (.test_empty(
        table = "data_score",
        batch_c = control,
        batch_o = object,
        location = .x
      )) {
        qry_object <- filter(
          gt.env$.tbl_object,
          .data$batch_c == control & .data$batch_o == object & .data$location == .x
        )
        qry_object <- collect(qry_object)
        if (nrow(qry_object) != 0) {
          qry_control <- filter(gt.env$.tbl_control, .data$batch == control & .data$location == .x)
          qry_control <- collect(qry_control)

          qry_control <- mutate(qry_control, date = as_date(.data$date))
          qry_object <- mutate(qry_object, date = as_date(.data$date))

          # adapt time series frequency
          qry_control <- .reset_date(qry_control)
          qry_object <- .reset_date(qry_object)

          if (
            min(
              nrow(count(qry_control, .data$date)),
              nrow(count(qry_object, .data$date))
            ) >= 24
          ) {
            # adjust to time series and impute negative values
            qry_control <- nest(qry_control, data = c(.data$date, .data$hits))
            qry_control <- mutate(qry_control, data = map(.data$data, .adjust_ts))
            qry_control <- unnest(qry_control, .data$data)
            qry_control <- mutate(qry_control,
              hits_trd = case_when(
                .data$hits_trd < 0 & .data$hits_sad < 0 ~ 0.1,
                .data$hits_trd < 0 ~ (.data$hits_obs + .data$hits_sad) / 2,
                TRUE ~ .data$hits_trd
              ),
              hits_sad = case_when(
                .data$hits_sad < 0 & .data$hits_trd < 0 ~ 0.1,
                .data$hits_sad < 0 ~ (.data$hits_obs + .data$hits_trd) / 2,
                TRUE ~ .data$hits_sad
              )
            )
            qry_object <- nest(qry_object, data = c(.data$date, .data$hits))
            qry_object <- mutate(qry_object, data = map(.data$data, .adjust_ts))
            qry_object <- unnest(qry_object, .data$data)
            qry_object <- mutate(qry_object,
              hits_trd = case_when(
                .data$hits_trd < 0 & .data$hits_sad < 0 ~ 0.1,
                .data$hits_trd < 0 ~ (.data$hits_obs + .data$hits_sad) / 2,
                TRUE ~ .data$hits_trd
              ),
              hits_sad = case_when(
                .data$hits_sad < 0 & .data$hits_trd < 0 ~ 0.1,
                .data$hits_sad < 0 ~ (.data$hits_obs + .data$hits_trd) / 2,
                TRUE ~ .data$hits_sad
              )
            )
          } else {
            if (nrow(count(qry_control, .data$date)) < 24) assign("ts_control", FALSE, envir = env_parent())
            if (nrow(count(qry_object, .data$date)) < 24) assign("ts_object", FALSE, envir = env_parent())
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
          data_control <- inner_join(
            qry_object,
            qry_control,
            by = c(
              "location",
              "keyword",
              "date",
              "key"
            ),
            suffix = c("_o", "_c")
          )
          data_control <- mutate(
            data_control,
            value_o = case_when(
              .data$value_o == 0 ~ 1,
              TRUE ~ .data$value_o
            ),
            value_c = case_when(
              .data$value_c == 0 ~ 1,
              TRUE ~ .data$value_c
            )
          )
          data_control <- mutate(data_control,
            benchmark = coalesce(.data$value_o / .data$value_c, 0)
          )
          data_control <- select(data_control, .data$location, .data$date, .data$key, .data$benchmark)
          data_control <- inner_join(
            data_control,
            qry_control,
            by = c("location", "date", "key")
          )
          data_control <- mutate(data_control, value = .data$value * .data$benchmark)
          data_control <- select(
            data_control,
            .data$location,
            .data$date,
            .data$key,
            .data$keyword,
            .data$value
          )

          data_object <- anti_join(qry_object, data_control, by = c("keyword"))

          # compute score
          data_control <- group_by(data_control, .data$location, .data$date, .data$key)
          data_control <- summarise(data_control, value_c = sum(.data$value), .groups = "drop")
          data_object <- left_join(
            data_object,
            data_control,
            by = c("location", "date", "key")
          )
          data_object <- mutate(data_object,
            score = coalesce(.data$value / .data$value_c, 0),
            key = str_replace(.data$key, "hits$", "score_obs"),
            key = str_replace(.data$key, "hits_", "score_")
          )
          data_object <- select(data_object, .data$location, .data$date, .data$keyword, .data$key, .data$score)
          out <- pivot_wider(
            data_object,
            names_from = .data$key,
            values_from = .data$score,
            values_fill = 0
          )
          out <- mutate(
            out,
            batch_c = control,
            batch_o = object,
            synonym = case_when(
              .data$keyword %in% gt.env$.keyword_synonyms$synonym ~ TRUE,
              TRUE ~ FALSE
            )
          )
          dbWriteTable(
            conn = gt.env$globaltrends_db,
            name = "data_score",
            value = out,
            append = TRUE
          )
        }
      }
      message(glue("Successfully computed search score | control: {control} | object: {object} | location: {.x} [{current}/{total}]", current = which(locations == .x), total = length(locations)))
    })
    .aggregate_synonym(object = object)
    if (!ts_control | !ts_object) {
      text <- case_when(
        all(!c(ts_control, ts_object)) ~ "control and object",
        first(!c(ts_control, ts_object)) ~ "control",
        last(!c(ts_control, ts_object)) ~ "object"
      )
      warning(glue("You provided {text} data for less than 24 months.\nNo time series adjustments possible."))
    }
  }
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(object, control = 1, locations = gt.env$countries) {
  walk(object, compute_score, control = control, locations = locations)
}

#' @rdname compute_score
#' @export

compute_voi <- function(object, control = 1) {
  compute_score(control = control, object = object, locations = "world")
}

#' @title Reset date
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom lubridate ymd
#' @importFrom rlang .data

.reset_date <- function(data) {
  out <- mutate(data, day = 1, month = month(.data$date), year = year(.data$date))
  out <- group_by(out, .data$location, .data$keyword, .data$year, .data$month, .data$day)
  out <- summarise(out, hits = mean(.data$hits), .groups = "drop")
  out <- mutate(out, date = ymd(glue("{.data$year}-{.data$month}-{.data$day}")))
  out <- select(out, .data$location, .data$keyword, .data$date, .data$hits)
  return(out)
}

#' @title Adjust time series
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom tibble tibble

.adjust_ts <- function(data) {
  myts <- stats::ts(data$hits, start = c(year(min(data$date)), month(min(data$date))), end = c(year(max(data$date)), month(max(data$date))), frequency = 12)
  fit <- stats::stl(myts, s.window = "period")
  trend <- fit$time.series[, "trend"]
  seasad <- forecast::seasadj(fit)
  out <- tibble(date = data$date, hits_obs = data$hits, hits_trd = as.double(trend), hits_sad = as.double(seasad))
  return(out)
}

#' Aggregate synonyms
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @export
#' @importFrom DBI dbExecute
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom purrr walk

.aggregate_synonym <- function(object) {
  lst_synonym <- filter(gt.env$.keywords_object, .data$batch == object)
  lst_synonym1 <- inner_join(lst_synonym, gt.env$.keyword_synonyms, by = "keyword")
  lst_synonym2 <- inner_join(lst_synonym, gt.env$.keyword_synonyms, by = c("keyword" = "synonym"))
  lst_synonym <- unique(c(lst_synonym1$synonym, lst_synonym2$keyword))

  if (length(lst_synonym) > 0) {
    message("Checking for synonyms...")
    data_synonym <- filter(gt.env$.tbl_score, .data$keyword %in% lst_synonym & .data$synonym == 1)
    data_synonym <- collect(data_synonym)

    if (nrow(data_synonym) > 0) {
      message("Aggregating scores for synonyms...")
      lst_main <- unique(gt.env$.keyword_synonyms$keyword[gt.env$.keyword_synonyms$synonym %in% lst_synonym])
      data_main <- filter(gt.env$.tbl_score, .data$keyword %in% lst_main)
      data_main <- collect(data_main)

      walk(lst_synonym, ~ {
        keyword_main <- gt.env$.keyword_synonyms$keyword[gt.env$.keyword_synonyms$synonym == .x][[1]]
        sub_main <- filter(data_main, .data$keyword == keyword_main)

        sub_synonym <- filter(data_synonym, .data$keyword == .x)
        sub_main <- left_join(
          sub_main,
          sub_synonym,
          by = c("location", "date", "batch_c"),
          suffix = c("", "_s")
        )

        sub_main <- mutate(
          sub_main,
          score_obs = .data$score_obs + coalesce(.data$score_obs_s, 0),
          score_sad = .data$score_sad + coalesce(.data$score_sad_s, 0),
          score_trd = .data$score_trd + coalesce(.data$score_trd_s, 0)
        )
        sub_main <- select(
          sub_main,
          .data$location,
          .data$keyword,
          .data$date,
          .data$score_obs,
          .data$score_sad,
          .data$score_trd,
          .data$batch_c,
          .data$batch_o,
          .data$synonym
        )

        data_synonym_agg <- inner_join(
          sub_synonym,
          select(
            sub_main,
            .data$location,
            .data$date,
            .data$batch_c
          ),
          by = c("location", "date", "batch_c")
        )
        data_synonym_agg <- mutate(data_synonym_agg, synonym = 2)
        data_synonym_nagg <- anti_join(
          sub_synonym,
          select(
            sub_main,
            location,
            date,
            batch_c
          ),
          by = c("location", "date", "batch_c")
        )

        data <- bind_rows(sub_main, data_synonym_agg, data_synonym_nagg)
        dbExecute(conn = gt.env$globaltrends_db, statement = "DELETE FROM data_score WHERE keyword=?", params = list(keyword_main))
        dbExecute(conn = gt.env$globaltrends_db, statement = "DELETE FROM data_score WHERE keyword=?", params = list(.x))
        dbWriteTable(conn = gt.env$globaltrends_db, name = "data_score", value = data, append = TRUE)
      })
    }
  }
}
