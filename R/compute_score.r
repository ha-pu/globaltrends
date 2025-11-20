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
#' When synonyms were specified through `add_synonym`, search
#' scores for synonyms are added to the main keyword.
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
#' * [stats::stl()]
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
#' @importFrom purrr map_dfr
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
  UseMethod("compute_score", object)
}

#' @rdname compute_score
#' @method compute_score numeric
#' @export

compute_score.numeric <- function(
  object,
  control = 1,
  locations = gt.env$countries
) {
  control <- unlist(control)
  .check_length(control, 1)
  .check_input(locations, "character")
  if (length(object) > 1) {
    compute_score(
      control = control,
      object = as.list(object),
      locations = locations
    )
  } else {
    walk(list(control, object), .check_batch)
    lst_full <- .get_full(
      table = "data_score",
      batch_c = control,
      batch_o = object
    )
    locations <- locations[!(locations %in% lst_full)]
    exp_object <- filter(
      gt.env$tbl_object,
      .data$batch_c == control &
        .data$batch_o == object
    )
    lst_synonyms <- collect(gt.env$keyword_synonyms)
    lst_synonyms <- lst_synonyms$synonym
    exp_object <- collect(exp_object)
    exp_control <- filter(gt.env$tbl_control, .data$batch == control)
    exp_control <- collect(exp_control)
    lst_out <- map_dfr(
      locations,
      ~ {
        qry_object <- filter(exp_object, .data$location == .x)
        if (nrow(qry_object) != 0) {
          qry_control <- filter(exp_control, .data$location == .x)

          # set to benchmark
          data_control <- inner_join(
            qry_object,
            qry_control,
            by = c(
              "location",
              "keyword",
              "date"
            ),
            suffix = c("_o", "_c"),
            relationship = "many-to-one"
          )
          data_control <- mutate(
            data_control,
            hits_o = case_when(
              .data$hits_o == 0 ~ 1,
              TRUE ~ .data$hits_o
            ),
            hits_c = case_when(
              .data$hits_c == 0 ~ 1,
              TRUE ~ .data$hits_c
            )
          )
          data_control <- mutate(
            data_control,
            benchmark = coalesce(.data$hits_o / .data$hits_c, 0)
          )
          data_control <- select(data_control, location, date, benchmark)
          data_control <- inner_join(
            data_control,
            qry_control,
            by = c("location", "date"),
            relationship = "many-to-many"
          )
          data_control <- mutate(
            data_control,
            hits = .data$hits * .data$benchmark
          )
          data_control <- select(
            data_control,
            location,
            date,
            keyword,
            hits
          )

          data_object <- anti_join(
            qry_object,
            data_control,
            by = c("keyword")
          )

          # compute score
          data_control <- group_by(data_control, .data$location, .data$date)
          data_control <- summarise(
            data_control,
            hits_c = sum(.data$hits),
            .groups = "drop"
          )
          data_object <- left_join(
            data_object,
            data_control,
            by = c("location", "date"),
            relationship = "many-to-one"
          )
          data_object <- mutate(
            data_object,
            score = coalesce(.data$hits / .data$hits_c, 0)
          )
          data_object <- select(data_object, location, date, keyword, score)
          out <- mutate(
            data_object,
            batch_c = control,
            batch_o = object,
            synonym = case_when(
              .data$keyword %in% lst_synonyms ~ TRUE,
              TRUE ~ FALSE
            ),
            date = as_date(date)
          )
          in_location <- .x
          message(paste0(
            "Successfully computed search score | control: ",
            control,
            " | object: ",
            object,
            " | location: ",
            in_location,
            " [",
            which(locations == .x),
            "/",
            length(locations),
            "]"
          ))
          return(out)
        }
      }
    )
    if (length(lst_out) > 0) {
      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "data_score",
        value = lst_out,
        append = TRUE
      )
    }
    # .aggregate_synonym(object = object)
  }
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(
  object,
  control = 1,
  locations = gt.env$countries
) {
  walk(object, compute_score, control = control, locations = locations)
}

#' @rdname compute_score
#' @export

compute_voi <- function(object, control = 1) {
  compute_score(control = control, object = object, locations = "world")
}

#' Aggregate synonyms
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @importFrom DBI dbExecute
#' @importFrom DBI dbAppendTable
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
  lst_synonym <- filter(gt.env$keywords_object, .data$batch == object)
  lst_synonym1 <- inner_join(
    lst_synonym,
    gt.env$keyword_synonyms,
    by = "keyword",
    multiple = "all"
  )
  lst_synonym2 <- inner_join(
    lst_synonym,
    gt.env$keyword_synonyms,
    by = c("keyword" = "synonym"),
    relationship = "many-to-one"
  )
  lst_synonym <- unique(c(lst_synonym1$synonym, lst_synonym2$keyword))

  if (length(lst_synonym) > 0) {
    message("Checking for synonyms...")
    data_synonym <- filter(
      gt.env$tbl_score,
      .data$keyword %in% lst_synonym & .data$synonym == 1
    )
    data_synonym <- collect(data_synonym)

    if (nrow(data_synonym) > 0) {
      message("Aggregating scores for synonyms...")
      lst_main <- unique(gt.env$keyword_synonyms$keyword[
        gt.env$keyword_synonyms$synonym %in% lst_synonym
      ])
      data_main <- filter(gt.env$tbl_score, .data$keyword %in% lst_main)
      data_main <- collect(data_main)

      walk(
        lst_synonym,
        ~ {
          keyword_main <- gt.env$keyword_synonyms$keyword[
            gt.env$keyword_synonyms$synonym == .x
          ][[1]]
          sub_main <- filter(data_main, .data$keyword == keyword_main)

          sub_synonym <- filter(data_synonym, .data$keyword == .x)
          sub_main <- left_join(
            sub_main,
            sub_synonym,
            by = c("location", "date", "batch_c"),
            suffix = c("", "_s"),
            relationship = "many-to-one"
          )

          sub_main <- mutate(
            sub_main,
            score = .data$score + coalesce(.data$score_s, 0)
          )
          sub_main <- select(
            sub_main,
            location,
            keyword,
            date,
            score,
            batch_c,
            batch_o,
            synonym
          )

          data_synonym_agg <- inner_join(
            sub_synonym,
            select(
              sub_main,
              location,
              date,
              batch_c
            ),
            by = c("location", "date", "batch_c"),
            relationship = "many-to-one"
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
          dbExecute(
            conn = gt.env$globaltrends_db,
            statement = "DELETE FROM data_score WHERE keyword=?",
            params = list(keyword_main)
          )
          dbExecute(
            conn = gt.env$globaltrends_db,
            statement = "DELETE FROM data_score WHERE keyword=?",
            params = list(.x)
          )
          dbAppendTable(
            conn = gt.env$globaltrends_db,
            name = "data_score",
            value = data
          )
        }
      )
    }
  }
}
