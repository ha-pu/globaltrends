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
#' aggregates all search volumes to monthly data. Next, it follows the procedure
#' outlined by Castelnuovo and Tran (2017, pp. A1-A2) to map control and object
#' data. After the mapping, object search volumes are divided by the sum of
#' control search volumes in the respective control batch. We use the sum of
#' search volumes for a set of control keywords, rather than the search volumes
#' for a single control keyword, to smooth-out variation in the underlying
#' control data.
#'
#' *Castelnuovo, E. & Tran, T. D. 2017. Google It Up! A Google Trends-based
#' Uncertainty index for the United States and Australia. Economics Letters,
#' 161: 149-153.*
#'
#' @section Note:
#' When synonyms were specified through `add_synonym`, run `aggregate_synonyms`
#' to add their search scores to the main term.
#'
#' @param control Control batch for which the data is downloaded. Object
#' of type `numeric`. Defaults to 1.
#'
#' @param object Object batch for which the data is downloaded. Object
#' of type `numeric` or object of type `list` containing single
#' objects of type `numeric`.
#'
#' @param locations List of countries or regions for which the data is
#' downloaded. Refers to lists generated in `start_db`. Defaults to
#' `countries`.
#'
#' @seealso
#' * [example_score()]
#' * [add_synonym()]
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
#' @importFrom DBI dbAppendTable
#' @importFrom DBI SQL
#' @importFrom dbplyr sql_render
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
#' @importFrom lubridate as_date
#' @importFrom purrr reduce
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom rlang env_parent
#' @importFrom stringr str_replace
#' @importFrom stringr str_sub
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest

compute_score <- function(object, control = 1, locations = gt.env$countries) {
  # preparation and checks
  control <- unlist(control)
  .check_length(control, 1)
  .check_input(locations, "character")

  # walk through batchs
  walk(
    object,
    ~ {
      walk(list(control, .x), .check_batch)
      lst_full <- .get_full(
        table = "data_score",
        batch_c = control,
        batch_o = .x
      )
      locations <- locations[!(locations %in% lst_full)]
      exp_object <- filter(
        gt.env$tbl_object,
        .data$batch_c == control &
          .data$batch_o == .x &
          .data$location %in% locations
      )
      exp_control <- filter(
        gt.env$tbl_control,
        .data$batch == control &
          .data$location %in% locations
      )
      if (pull(count(exp_object), n) != 0) {
        data_control <- exp_object |>
          inner_join(
            exp_control,
            by = c(
              "location",
              "keyword",
              "date"
            ),
            suffix = c("_o", "_c")
          ) |>
          mutate(
            hits_o = case_when(
              .data$hits_o == 0 ~ 1,
              TRUE ~ .data$hits_o
            ),
            hits_c = case_when(
              .data$hits_c == 0 ~ 1,
              TRUE ~ .data$hits_c
            )
          ) |>
          mutate(
            benchmark = coalesce(.data$hits_o / .data$hits_c, 0)
          ) |>
          select(
            location,
            date,
            benchmark
          ) |>
          inner_join(
            exp_control,
            by = c("location", "date"),
            relationship = "many-to-many"
          ) |>
          mutate(
            hits = .data$hits * .data$benchmark
          ) |>
          select(
            location,
            date,
            keyword,
            hits
          ) |>
          summarise(
            hits_c = sum(.data$hits, na.rm = TRUE),
            .by = c(location, date)
          )

        data_object <- exp_object |>
          anti_join(
            exp_control,
            by = c("keyword")
          ) |>
          left_join(
            data_control,
            by = c("location", "date")
          ) |>
          mutate(
            score = coalesce(.data$hits / .data$hits_c, 0)
          ) |>
          select(
            location,
            date,
            keyword,
            score
          )

        out <- mutate(
          data_object,
          batch_c = control,
          batch_o = .x
        ) |>
          select(
            location,
            keyword,
            date,
            score,
            batch_c,
            batch_o
          )

        message(paste0(
          "Successfully computed search score | control: ",
          control,
          " | object: ",
          .x,
          "]"
        ))

        sql_lazy <- sql_render(out)

        if (pull(count(out), n) > 0) {
          dbExecute(
            gt.env$globaltrends_db,
            SQL(paste0("INSERT INTO data_score ", sql_lazy))
          )
        }
      }
    }
  )
  dbExecute(
    gt.env$globaltrends_db,
    "COPY data_score TO 'db/data_score.parquet' (FORMAT parquet);"
  )
}

#' @rdname compute_score
#' @export

compute_voi <- function(object, control = 1) {
  compute_score(control = control, object = object, locations = "world")
}
