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
#'   batches (`batch_o`).
#'   \item Pulls the relevant `data_score` rows, remaps synonym rows onto their
#'   canonical keyword, and sums scores across duplicates.
#'   \item Deletes the affected `data_score` rows for those object batches.
#'   \item Writes the aggregated rows back to `data_score`.
#' }
#'
#' @param control Numeric/integer scalar. The control batch id (`batch_c`),
#'   identifying the reference search used for score normalisation. In most
#'   single-control setups this is `1`.
#'
#' @return
#' Invisibly returns a data frame of the rows written to `data_score`. Called
#' primarily for its side effects (data modifications).
#'
#' @seealso
#' [compute_score()] to populate `data_score` before aggregating,
#' [add_synonym()] to define synonym relationships,
#'
#' @examples
#' \dontrun{
#' compute_score(object = 1:2, control = 1)
#' aggregate_synonyms(control = 1)
#' }
#'
#' @export
#' @rdname aggregate_synonyms

aggregate_synonyms <- function(control) {
  .check_length(control, 1)
  .check_batch(control)

  syn <- gt.env$dt_synonyms
  bk <- gt.env$dt_keywords[gt.env$dt_keywords$type == "object", ]

  syn_map <- merge(syn, bk, by.x = "keyword", by.y = "keyword")
  if (nrow(syn_map) == 0L) {
    message("No synonym mappings found in the database. Nothing to aggregate.")
    return(invisible(data.frame()))
  }
  names(syn_map)[names(syn_map) == "batch"] <- "batch_o_canonical"
  syn_map <- syn_map[, c("keyword", "synonym", "batch_o_canonical")]
  names(syn_map)[1] <- "keyword_canonical"
  names(syn_map)[2] <- "keyword_synonym"

  syn_map2 <- merge(syn_map, bk, by.x = "keyword_synonym", by.y = "keyword")
  names(syn_map2)[names(syn_map2) == "batch"] <- "batch_o_synonym"
  syn_map2 <- syn_map2[, c(
    "keyword_canonical", "keyword_synonym",
    "batch_o_canonical", "batch_o_synonym"
  )]

  if (nrow(syn_map2) == 0L) {
    message("No synonym mappings found in the database. Nothing to aggregate.")
    return(invisible(data.frame()))
  }

  affected_batches <- unique(c(
    syn_map2$batch_o_canonical,
    syn_map2$batch_o_synonym
  ))

  dt_s <- gt.env$dt_score
  score_tbl <- as.data.frame(
    dt_s[dt_s$batch_c == control & dt_s$batch_o %in% affected_batches, ]
  )
  if ("date" %in% names(score_tbl)) {
    score_tbl$date <- as.Date(score_tbl$date)
  }

  if (nrow(score_tbl) == 0) {
    message(
      "No score data found for the specified control batch and affected object batches."
    )
    return(invisible(data.frame()))
  }

  merged <- merge(
    syn_map2,
    score_tbl,
    by.x = c("batch_o_synonym", "keyword_synonym"),
    by.y = c("batch_o", "keyword")
  )

  score_syn_rolled <- stats::aggregate(
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

  syn_key <- paste(syn_map2$batch_o_synonym, syn_map2$keyword_synonym)
  score_non_syn <- score_tbl[
    !paste(score_tbl$batch_o, score_tbl$keyword) %in% syn_key,
  ]

  score_combined <- rbind(score_non_syn, score_syn_rolled)
  score_new <- stats::aggregate(
    score ~ batch_o + keyword + location + date + batch_c,
    data = score_combined,
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  if (nrow(score_new) == 0) {
    message("Aggregation produced no rows. No database changes were made.")
    return(invisible(data.frame()))
  }

  for (batch in affected_batches) {
    remove_data(table = "data_score", control = control, object = batch)
  }

  data.table::setDT(score_new)
  gt.env$dt_score <- data.table::rbindlist(
    list(gt.env$dt_score, score_new),
    use.names = TRUE
  )
  # `rbindlist()` above drops the key set in `start_db()`. Re-key so the
  # next `.get_full()` call can binary search instead of scanning the full
  # table.
  data.table::setkey(gt.env$dt_score, batch_c, batch_o, location)

  message(
    "Successfully aggregated synonyms into canonical keywords for data_score."
  )

  invisible(as.data.frame(score_new))
}
