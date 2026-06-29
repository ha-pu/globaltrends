#' @title Compute search scores for object keywords
#'
#' @description
#' Computes *search scores* for object keywords by mapping object and control
#' search volumes onto a common scale and then normalizing object volumes by the
#' mapped control total for each `(location, date)`.
#'
#' @details
#' Conceptually, the score for an object keyword is computed as:
#' \deqn{
#'   score_{o,loc,t} = \frac{hits_{o,loc,t}}{\sum_{k \in C} \tilde{hits}_{k,loc,t}}
#' }
#' where \eqn{C} is the set of control keywords and \eqn{\tilde{hits}} are control
#' hits mapped to the object scale using an overlap-based benchmark, following
#' the mapping logic described in Castelnuovo and Tran (2017, Appendix A).
#'
#' **Idempotency.** Already-computed `(batch_c, batch_o, location)` combinations
#' are detected and skipped automatically, so repeated calls safely fill in only
#' missing locations.
#'
#' Operationally, for each object batch (`batch_o`) and control batch (`batch_c`),
#' the function:
#' \enumerate{
#'   \item Identifies the subset of `locations` not yet present in `data_score`
#'   for this `(batch_c, batch_o)` pair.
#'   \item Computes a per-`(location, date)` *benchmark* as the mean ratio of
#'   object-to-control hits for the keywords that appear in both downloads.
#'   \item Maps control hits to the object scale: `hits_mapped = hits * benchmark`.
#'   \item Sums mapped control hits across keywords to obtain `hits_c` and
#'   computes `score = hits_object / hits_c` for each object keyword.
#'   \item Inserts the resulting rows into `data_score`.
#' }
#'
#' If synonym keywords were specified via [add_synonym()], run
#' [aggregate_synonyms()] after score computation to roll synonym scores into
#' their canonical terms.
#'
#' @param object Integer-like scalar, vector, or list. The object batch id(s)
#'   (`batch_o`) for which scores should be computed. A numeric vector is
#'   coerced to a list and each element is processed in sequence; a list
#'   triggers the list method directly.
#'
#' @param control Integer-like scalar. The control batch id (`batch_c`) used as
#'   the normalisation baseline. Defaults to `1`.
#'
#' @param locations Character vector of location codes to compute scores for.
#'   The package exports `countries` (ISO 3166-1 alpha-2 codes for all
#'   countries) and `us_states` (two-letter US state codes) as convenience
#'   vectors. Pass `"world"` to compute the global aggregate only (see also
#'   [compute_voi()]). If `NULL`, defaults to `gt.env$countries` when set via
#'   [start_db()], otherwise falls back to `globaltrends::countries`.
#'
#' @return
#' Called primarily for its side effects (writing to `data_score`); the return
#' value is rarely needed. When `object` is a scalar or vector, returns the
#' number of rows inserted into `data_score` as an integer (`0L` if all
#' requested locations were already computed). When `object` is a list,
#' returns `TRUE` invisibly after processing all elements.
#'
#' @seealso
#' [download_control()] and [download_object()] to populate the raw data tables
#' before computing scores.
#' [aggregate_synonyms()] to roll synonym keyword scores into their canonical
#' terms after score computation.
#' [add_synonym()] to define synonym relationships.
#' [compute_voi()] for the global-aggregate shorthand.
#'
#' @references
#' Castelnuovo, E. & Tran, T. D. (2017). Google It Up! A Google Trends-based
#' Uncertainty index for the United States and Australia. *Economics Letters*,
#' *161*, 149--153. \doi{10.1016/j.econlet.2017.09.032}
#'
#' @examples
#' \dontrun{
#' # Compute scores for a single object batch across all countries
#' compute_score(object = 1, control = 1, locations = countries)
#'
#' # Process multiple object batches in one call
#' compute_score(object = as.list(1:5), control = 1, locations = countries)
#'
#' # Compute the global aggregate (VOI) only
#' compute_voi(object = 1, control = 1)
#' }
#'
#' @export
#' @rdname compute_score

compute_score <- function(object, control = 1, locations = NULL) {
  UseMethod("compute_score", object)
}

#' @rdname compute_score
#' @method compute_score numeric
#' @export

compute_score.numeric <- function(object, control = 1, locations = NULL) {
  args <- .resolve_score_args(control, locations)
  control <- args$control
  locations <- args$locations

  if (length(object) > 1) {
    compute_score(
      object = as.list(object),
      control = control,
      locations = locations
    )
    return(invisible(TRUE))
  }

  .check_batch(object)

  existing_locations <- .get_full(
    table = "data_score",
    in_batch_c = control,
    in_batch_o = object
  )
  loc_remaining <- locations[!(locations %in% existing_locations)]

  if (length(loc_remaining) == 0) {
    message(sprintf(
      "No new locations to compute | control: %s | object: %s.",
      control,
      object
    ))
    return(invisible(0L))
  }

  dt_obj_all <- gt.env$dt_object
  dt_ctrl_all <- gt.env$dt_control

  dt_obj <- dt_obj_all[
    dt_obj_all$batch_c == control &
    dt_obj_all$batch_o == object &
    dt_obj_all$location %in% loc_remaining,
  ]

  if (nrow(dt_obj) == 0L) {
    message(sprintf(
      "No object data found | control: %s | object: %s.",
      control,
      object
    ))
    return(invisible(0L))
  }

  dt_ctrl <- dt_ctrl_all[
    dt_ctrl_all$batch == control &
    dt_ctrl_all$location %in% loc_remaining,
  ]

  ctrl_kws <- unique(dt_ctrl$keyword)

  # Overlap keywords: present in both control and object
  overlap <- dt_obj[dt_obj$keyword %in% ctrl_kws, ]

  # Benchmark: mean(o_hits / c_hits) per (location, date)
  bm <- merge(
    overlap[, c("location", "keyword", "date", "hits")],
    dt_ctrl[, c("location", "keyword", "date", "hits")],
    by = c("location", "keyword", "date"),
    suffixes = c("_o", "_c")
  )
  bm$hits_o_safe <- ifelse(
    is.na(bm$hits_o) | bm$hits_o == 0, 1, bm$hits_o
  )
  bm$hits_c_safe <- ifelse(
    is.na(bm$hits_c) | bm$hits_c == 0, 1, bm$hits_c
  )
  bm$ratio <- bm$hits_o_safe / bm$hits_c_safe

  benchmark <- stats::aggregate(
    ratio ~ location + date,
    data = bm,
    FUN = mean
  )
  names(benchmark)[names(benchmark) == "ratio"] <- "benchmark"

  # Control mass: sum(c_hits * benchmark) per (location, date)
  ctrl_mapped <- merge(
    dt_ctrl[, c("location", "keyword", "date", "hits")],
    benchmark,
    by = c("location", "date")
  )
  ctrl_mapped$hits_mapped <- ctrl_mapped$hits * ctrl_mapped$benchmark

  mass <- stats::aggregate(
    hits_mapped ~ location + date,
    data = ctrl_mapped,
    FUN = sum
  )
  names(mass)[names(mass) == "hits_mapped"] <- "hits_c"

  # Scores: o_hits / control_mass for non-control object keywords
  obj_only <- dt_obj[!(dt_obj$keyword %in% ctrl_kws), ]
  scores <- merge(
    obj_only[, c("location", "keyword", "date", "hits")],
    mass,
    by = c("location", "date"),
    all.x = TRUE
  )
  scores$score <- ifelse(
    is.na(scores$hits_c) | scores$hits_c <= 0,
    0,
    ifelse(is.na(scores$hits), 0, scores$hits) / scores$hits_c
  )
  scores <- scores[, c("location", "keyword", "date", "score")]
  scores$batch_c <- control
  scores$batch_o <- object

  data.table::setDT(scores)
  gt.env$dt_score <- data.table::rbindlist(
    list(gt.env$dt_score, scores),
    use.names = TRUE
  )

  n_out <- nrow(scores)

  message(sprintf(
    "Successfully computed search scores | control: %s | object: %s.",
    control,
    object
  ))

  invisible(as.integer(n_out))
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(object, control = 1, locations = NULL) {
  args <- .resolve_score_args(control, locations)
  for (o in object) {
    compute_score(o, control = args$control, locations = args$locations)
  }
  invisible(TRUE)
}

#' @title Compute volume of internationalization (VOI)
#'
#' @description
#' Convenience wrapper around [compute_score()] for computing the *volume of
#' internationalization* (VOI) — a measure of how globally distributed search
#' interest for a keyword is relative to the control baseline. Equivalent to
#' `compute_score(object, control, locations = "world")`, which uses the
#' worldwide aggregate rather than country-level breakdowns.
#'
#' Use this function when you only need the global aggregate score, for example
#' when `locations = "world"` was passed to [download_object()].
#'
#' @param object Integer-like scalar, vector, or list. The object batch id(s)
#'   (`batch_o`) for which VOI should be computed.
#' @param control Integer-like scalar. The control batch id (`batch_c`).
#'   Defaults to `1`.
#'
#' @return See [compute_score()] for return value semantics.
#'
#' @seealso [compute_score()] for country-level scores.
#'
#' @export
#' @rdname compute_score

compute_voi <- function(object, control = 1) {
  compute_score(object = object, control = control, locations = "world")
}

#' @noRd
.resolve_score_args <- function(control, locations) {
  control <- unlist(control)
  .check_length(control, 1)
  .check_batch(control)
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) {
      gt.env$countries
    } else {
      globaltrends::countries
    }
  }
  .check_input(locations, "character")
  list(control = control, locations = locations)
}
