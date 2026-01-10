#' @title Aggregate search scores across synonym terms
#'
#' @description
#' Aggregates score time series for object keywords by rolling up scores from
#' synonym keywords (defined via [add_synonym()] and synonym table maintenance)
#' into their canonical ("main") keywords.
#'
#' @details
#' This function is intended to be run **after** [compute_score()] has populated
#' `data_score`. For a given `control` batch (`batch_c`), it:
#' \enumerate{
#'   \item Identifies affected object batches (`batch_o`) that contain either a
#'   canonical keyword or a synonym keyword.
#'   \item Computes aggregated scores by mapping synonym rows onto their
#'   canonical keyword and summing over duplicates.
#'   \item Deletes existing `data_score` rows for the affected object batches
#'   (greedy cleanup is not required here because only `data_score` is modified).
#'   \item Writes the aggregated rows back to `data_score`.
#'   \item Optionally runs [vacuum_data()] to reclaim disk space after deletions.
#' }
#'
#' Performance note: Deleting and re-inserting can be expensive for large
#' datasets. Vacuuming is the most time-consuming step and is therefore
#' optional.
#'
#' @param control Numeric/integer scalar. Control batch id (`batch_c`) whose
#'   score data should be aggregated. Typically `1`.
#'
#' @param vacuum Logical scalar. If `TRUE` (default), calls [vacuum_data()]
#'   after aggregation to reclaim space.
#'
#' @return
#' Invisibly returns a tibble of the rows written to `data_score`. The function
#' is called for its side effects (modifying the database) and emits informative
#' messages.
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
#' @importFrom dplyr anti_join collect distinct filter inner_join rename select summarise union_all count
#' @importFrom purrr walk
#' @importFrom rlang .data

aggregate_synonyms <- function(control, vacuum = TRUE) {
  .check_length(control, 1)
  .check_batch(control)
  .check_input(vacuum, "logical")
  .check_length(vacuum, 1)

  # -----------------------------------------------------------------------
  # 1) Build mappings between canonical keywords and synonyms, including
  #    their respective object batches (batch_o) from batch_keywords.
  # -----------------------------------------------------------------------
  # Canonical keyword (keyword) -> synonym (synonym) with batch for canonical
  org_syn <- gt.env$tbl_keywords |>
    inner_join(gt.env$tbl_synonyms, by = "keyword") |>
    collect()

  # Synonym (synonym) -> canonical keyword (keyword) with batch for synonym keyword
  syn_org <- gt.env$tbl_synonyms |>
    inner_join(gt.env$tbl_keywords, by = c("synonym" = "keyword")) |>
    collect()

  # If there are no synonym definitions or no matching batches, exit early.
  if (nrow(org_syn) == 0 || nrow(syn_org) == 0) {
    message("No synonym mappings found in the database. Nothing to aggregate.")
    return(invisible(tibble()))
  }

  affected_batches <- unique(c(org_syn$batch, syn_org$batch))
  if (length(affected_batches) == 0) {
    message("No affected object batches found. Nothing to aggregate.")
    return(invisible(tibble()))
  }

  # -----------------------------------------------------------------------
  # 2) Pull relevant score rows (lazy until collect) for affected object batches
  #    under the specified control batch.
  # -----------------------------------------------------------------------
  score_tbl <- gt.env$tbl_score |>
    filter(.data$batch_c == control, .data$batch_o %in% affected_batches) |>
    collect()

  # If there is no score data, exit early (avoid delete + append).
  if (count(score_tbl) |> collect() |> pull(.data$n) == 0) {
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
  # Create a mapping table: synonym-batch+synonym-keyword -> canonical-batch+canonical-keyword
  # Note: joins here are performed in-memory (small mapping tables).
  syn_map <- syn_org |>
    inner_join(org_syn, by = c("keyword", "synonym")) |>
    select(
      batch_o_canonical = batch.y,
      keyword_canonical = keyword,
      batch_o_synonym = batch.x,
      keyword_synonym = synonym
    )

  # Pull score rows for synonym keywords and re-label to canonical keyword/batch
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

  # Keep all non-synonym rows as-is (i.e., exclude rows where keyword is a synonym)
  score_non_syn <- score_tbl |>
    anti_join(
      syn_org |> distinct(batch = .data$batch, synonym = .data$synonym),
      by = c("batch_o" = "batch", "keyword" = "synonym")
    )

  # Combine and de-duplicate by summing (canonical keyword may already exist)
  score_new <- union_all(score_non_syn, score_syn_rolled) |>
    summarise(
      score = sum(.data$score, na.rm = TRUE),
      .by = c(
        batch_o,
        keyword,
        location,
        date,
        batch_c
      )
    ) |>
    collect()

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
