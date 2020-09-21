#' Aggregate synonyms
#'
#' @keywords internal
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
#' @importFrom purrr walk

.aggregate_synonym <- function() {
  data_score <- filter(.tbl_score, synonym)
  data_score <- collect(data_score)
  
  if (nrow(data_score) > 0) {
    walk(unique(data_score$keyword), ~{
      keyword_main <- .keyword_synonyms$keyword[.keyword_synonyms$synonym == .x]
      data_main <- filter(.tbl_score, keyword == keyword_main)
      data_main <- collect(data_main)
      
      data_synonym <- filter(data_score, keyword == .x)
      data_main <- left_join(
        data_main,
        data_synonym,
        by = c("location", "date", "batch_c"),
        suffix = c("", "_s")
      )
      data_main <- mutate(data_main,
                          score_obs = score_obs + coalesce(score_obs_s, 0),
                          score_sad = score_sad + coalesce(score_sad_s, 0),
                          score_trd = score_trd + coalesce(score_trd_s, 0)
      )
      data_main <- select(
        data_main,
        location,
        keyword,
        date,
        score_obs,
        score_sad,
        score_trd,
        batch_c,
        batch_o,
        synonym
      )
      
      data_synonym_agg <- inner_join(
        data_synonym,
        select(data_main, location, date, batch_c),
        by = c("location", "date", "batch_c")
      )
      data_synonym_agg <- mutate(data_synonym_agg, synonym = 0)
      data_synonym_nagg <- anti_join(
        data_synonym,
        select(data_main, location, date, batch_c),
        by = c("location", "date", "batch_c")
      )
      
      data <- bind_rows(data_main, data_synonym_agg, data_synonym_nagg)
      dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE keyword=?", params = list(keyword_main))
      dbExecute(conn = globaltrends_db, statement = "DELETE FROM data_score WHERE keyword=?", params = list(.x))
      dbWriteTable(conn = globaltrends_db, name = "data_score", value = data, append = TRUE)
    })
  }
}
