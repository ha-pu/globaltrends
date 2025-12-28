#' @title Aggregate search scores for synonym terms
#'
#' @description
#' The function aggregates search scores for object keywords across synonyms
#' defined through `add_synonyms`. Users should run this funciton **after**
#' running `compute_score`. Then, search scores will be added and the synonym
#' terms removed from the *data_score* table. For efficiency reasons, the
#' function uses `purrr::map` to iterate across `batch_o`.
#'
#' @details
#' The function proceeds in five steps:
#'
#' 1. Export data from the database using `purrr::map_dfr`
#' 2. Aggreagate score data from synonyms to main term
#' 3. Remove batches that include synonyms from the database using `purrr::walk`
#' 4. If `vacuum = TRUE`, run `vacuum_data()` on the database
#' 5. Write aggregated data to the database using `purrr::walk`
#'
#' During these steps, `map_dfr` and `walk` calls return progress information
#' through `.progress = TRUE`.
#'
#' @section Note:
#' The aggregation of synonyms can be very time-consuming when using a large
#' dataset! Particulalry, the `vacuum_data()` call can require substantial time
#' to run.
#'
#' @param control Control batch for which the data is downloaded. Object
#' of type `numeric`. Defaults to 1.
#'
#' @param vacuum Indicator whether the function should call `vacuum_data()`.
#' Object of type `logical`. Defaults to `TRUE`.
#'
#' @seealso
#' * [add_synonym()]
#' * [vacuum_data()]
#'
#' @return Message that the aggregation of synonyms is complete. During the
#' aggregation process, messages regarding each step. The function shows the
#' `purrr::map_dfr` and `purrr:walk` progress bars.
#'
#' @examples
#' \dontrun{
#' compute_score(object = 1:2, control = 1)
#' aggregate_synonyms(control = 1, vacuum = FALSE)
#' }
#'
#' @export
#' @rdname aggregate_synonyms
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr bind_rows
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr union_all
#' @importFrom purrr map_dfr
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom rlang env_parent

aggregate_synonyms <- function(control, vacuum = TRUE) {
  .check_length(control, 1)
  .check_batch(control)
  .check_input(vacuum, "logical")

  lst_org_syn <- inner_join(
    gt.env$tbl_keywords,
    gt.env$tbl_synonyms,
    by = "keyword"
  )
  lst_org_syn <- collect(lst_org_syn)

  lst_syn_org <- inner_join(
    gt.env$tbl_synonyms,
    gt.env$tbl_keywords,
    by = c("synonym" = "keyword")
  )
  lst_syn_org <- collect(lst_syn_org)

  lst_batch <- c(
    pull(lst_org_syn, batch),
    pull(lst_syn_org, batch)
  )
  lst_batch <- unique(lst_batch)

  df_score <- filter(
    gt.env$tbl_score,
    .data$batch_o %in% lst_batch & .data$batch_c == control
  )

  df_score_s <- inner_join(
    lst_syn_org,
    lst_org_syn,
    by = c("keyword", "synonym")
  )
  df_score_s <- select(
    df_score_s,
    batch_oo = batch.y,
    keyword_o = keyword,
    batch_os = batch.x,
    keyword_s = synonym
  )
  df_score_s <- inner_join(
    df_score_s,
    df_score,
    by = c("batch_os" = "batch_o", "keyword_s" = "keyword")
  )
  df_score_s <- summarise(
    df_score_s,
    score = sum(.data$score, na.rm = TRUE),
    .by = c(
      batch_oo,
      keyword_o,
      location,
      date,
      batch_c
    )
  )
  df_score_s <- rename(
    df_score_s,
    batch_o = batch_oo,
    keyword = keyword_o
  )

  df_score_o <- anti_join(
    df_score,
    lst_syn_org,
    by = c("batch_o" = "batch", "keyword" = "synonym")
  )

  df_score_new <- union_all(
    df_score_o,
    df_score_s
  )
  df_score_new <- summarise(
    df_score_new,
    score = sum(.data$score, na.rm = TRUE),
    .by = c(
      batch_o,
      keyword,
      location,
      date,
      batch_c
    )
  )

  walk(
    lst_batch,
    ~ remove_data(table = "data_score", control = control, object = .x),
    .progress = TRUE
  )

  if (pull(count(df_score_new), n) > 0) {
    dbAppendTable(
      conn = gt.env$globaltrends_db,
      name = "data_score",
      value = df_score_new
    )
  }

  message("Successfully aggregated synonyms.")

  if (vacuum) {
    message("Start vacuum_data().")
    vacuum_data()
  }
}
