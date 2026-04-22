#' @title Aggregate search scores across synonym terms
#'
#' @description
#' Merges synonym keyword scores into their canonical keyword scores in
#' `data_score`. Run this after [compute_score()]. Synonym relationships are
#' defined with [add_synonym()].
#'
#' @details
#' For a given `control` batch (`batch_c`), this function:
#' \enumerate{
#'   \item Retrieves all canonical-synonym pairs and their associated object
#'   batches (`batch_o`) in a single database query.
#'   \item Pulls the relevant `data_score` rows, remaps synonym rows onto their
#'   canonical keyword, and sums scores across duplicates.
#'   \item Deletes the affected `data_score` rows for those object batches.
#'   \item Writes the aggregated rows back to `data_score`.
#'   \item Optionally calls [vacuum_data()] to reclaim disk space.
#' }
#'
#' The delete-and-reinsert pattern can be slow for large datasets. Vacuuming
#' adds the most overhead and can be deferred by setting `vacuum = FALSE`.
#'
#' @param control Numeric/integer scalar. The control batch id (`batch_c`),
#'   identifying the reference search used for score normalisation. In most
#'   single-control setups this is `1`.
#'
#' @param vacuum Logical scalar. If `TRUE` (default), calls [vacuum_data()]
#'   after aggregation to reclaim space freed by the row deletions.
#'
#' @return
#' Invisibly returns a tibble of the rows written to `data_score`. Called
#' primarily for its side effects (database modifications).
#'
#' @seealso
#' [compute_score()] to populate `data_score` before aggregating,
#' [add_synonym()] to define synonym relationships,
#' [vacuum_data()] for manual space reclamation.
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
#' @importFrom dplyr anti_join collect distinct filter inner_join rename select summarise union_all
#' @importFrom purrr walk
#' @importFrom rlang .data

aggregate_synonyms <- function(control, vacuum = TRUE) {
  .check_length(control, 1)
  .check_batch(control)
  .check_input(vacuum, "logical")
  .check_length(vacuum, 1)

  # -----------------------------------------------------------------------
  # 1) Build canonical<->synonym mapping with both object batches in one query.
  # -----------------------------------------------------------------------
  syn_map <- gt.env$tbl_synonyms |>
    inner_join(gt.env$tbl_keywords, by = "keyword") |>
    inner_join(gt.env$tbl_keywords, by = c("synonym" = "keyword"), suffix = c("_canonical", "_synonym")) |>
    select(
      keyword_canonical = keyword,
      keyword_synonym = synonym,
      batch_o_canonical = batch_canonical,
      batch_o_synonym = batch_synonym
    ) |>
    collect()

  if (nrow(syn_map) == 0) {
    message("No synonym mappings found in the database. Nothing to aggregate.")
    return(invisible(tibble()))
  }

  affected_batches <- unique(c(syn_map$batch_o_canonical, syn_map$batch_o_synonym))

  # -----------------------------------------------------------------------
  # 2) Pull relevant score rows for affected object batches.
  # -----------------------------------------------------------------------
  score_tbl <- gt.env$tbl_score |>
    filter(.data$batch_c == control, .data$batch_o %in% affected_batches) |>
    collect()

  if (nrow(score_tbl) == 0) {
    message(
      "No score data found for the specified control batch and affected object batches."
    )
    return(invisible(tibble()))
  }

  # -----------------------------------------------------------------------
  # 3) Compute synonym rollups:
  #    - Map synonym keyword rows to the canonical keyword
  #    - Sum scores by (batch_o, keyword, location, date, batch_c)
  # -----------------------------------------------------------------------

  score_syn_rolled <- syn_map |>
    inner_join(
      score_tbl,
      by = c("batch_o_synonym" = "batch_o", "keyword_synonym" = "keyword")
    ) |>
    summarise(
      score = sum(.data$score, na.rm = TRUE),
      .by = c(
        batch_o_canonical,
        keyword_canonical,
        location,
        date,
        batch_c
      )
    ) |>
    rename(
      batch_o = batch_o_canonical,
      keyword = keyword_canonical
    )

  score_non_syn <- score_tbl |>
    anti_join(
      distinct(syn_map, batch = batch_o_synonym, keyword = keyword_synonym),
      by = c("batch_o" = "batch", "keyword")
    )

  score_new <- union_all(score_non_syn, score_syn_rolled) |>
    summarise(
      score = sum(.data$score, na.rm = TRUE),
      .by = c(batch_o, keyword, location, date, batch_c)
    )

  if (nrow(score_new) == 0) {
    message("Aggregation produced no rows. No database changes were made.")
    return(invisible(tibble()))
  }

  # -----------------------------------------------------------------------
  # 4) Replace affected rows in `data_score`
  # -----------------------------------------------------------------------
  walk(
    affected_batches,
    ~ remove_data(table = "data_score", control = control, object = .x),
    .progress = TRUE
  )

  dbAppendTable(
    conn = gt.env$globaltrends_db,
    name = "data_score",
    value = score_new
  )

  message(
    "Successfully aggregated synonyms into canonical keywords for data_score."
  )

  # -----------------------------------------------------------------------
  # 5) Optional vacuum
  # -----------------------------------------------------------------------
  if (isTRUE(vacuum)) {
    message("Running vacuum_data() to reclaim disk space.")
    vacuum_data()
  }

  invisible(score_new)
}
