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
#' Invisibly returns a data frame of the rows written to `data_score`. Called
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

aggregate_synonyms <- function(control, vacuum = TRUE) {
  .check_length(control, 1)
  .check_batch(control)
  .check_input(vacuum, "logical")
  .check_length(vacuum, 1)

  con <- gt.env$globaltrends_db

  # -----------------------------------------------------------------------
  # 1) Build canonical<->synonym mapping with both object batches in one query.
  # -----------------------------------------------------------------------
  syn_map <- DBI::dbGetQuery(
    con,
    "SELECT ks.keyword AS keyword_canonical,
            ks.synonym AS keyword_synonym,
            bk_c.batch AS batch_o_canonical,
            bk_s.batch AS batch_o_synonym
     FROM keyword_synonyms ks
     INNER JOIN batch_keywords bk_c ON bk_c.keyword = ks.keyword  AND bk_c.type = 'object'
     INNER JOIN batch_keywords bk_s ON bk_s.keyword = ks.synonym AND bk_s.type = 'object'"
  )

  if (nrow(syn_map) == 0) {
    message("No synonym mappings found in the database. Nothing to aggregate.")
    return(invisible(data.frame()))
  }

  affected_batches <- unique(c(
    syn_map$batch_o_canonical,
    syn_map$batch_o_synonym
  ))
  batch_in <- paste(affected_batches, collapse = ", ")

  # -----------------------------------------------------------------------
  # 2) Pull relevant score rows for affected object batches.
  # -----------------------------------------------------------------------
  score_tbl <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM data_score WHERE batch_c = %d AND batch_o IN (%s)",
      control,
      batch_in
    )
  )
  score_tbl$date <- as.Date(score_tbl$date)

  if (nrow(score_tbl) == 0) {
    message(
      "No score data found for the specified control batch and affected object batches."
    )
    return(invisible(data.frame()))
  }

  # -----------------------------------------------------------------------
  # 3) Compute synonym rollups.
  # -----------------------------------------------------------------------
  merged <- merge(
    syn_map,
    score_tbl,
    by.x = c("batch_o_synonym", "keyword_synonym"),
    by.y = c("batch_o", "keyword")
  )

  score_syn_rolled <- aggregate(
    score ~ batch_o_canonical + keyword_canonical + location + date + batch_c,
    data = merged,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  names(score_syn_rolled)[
    names(score_syn_rolled) == "batch_o_canonical"
  ] <- "batch_o"
  names(score_syn_rolled)[
    names(score_syn_rolled) == "keyword_canonical"
  ] <- "keyword"

  syn_key <- paste(syn_map$batch_o_synonym, syn_map$keyword_synonym)
  score_non_syn <- score_tbl[
    !paste(score_tbl$batch_o, score_tbl$keyword) %in% syn_key,
  ]

  score_combined <- rbind(score_non_syn, score_syn_rolled)
  score_new <- aggregate(
    score ~ batch_o + keyword + location + date + batch_c,
    data = score_combined,
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  if (nrow(score_new) == 0) {
    message("Aggregation produced no rows. No database changes were made.")
    return(invisible(data.frame()))
  }

  # -----------------------------------------------------------------------
  # 4) Replace affected rows in `data_score`
  # -----------------------------------------------------------------------
  for (batch in affected_batches) {
    remove_data(table = "data_score", control = control, object = batch)
  }

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
