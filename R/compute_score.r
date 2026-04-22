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
#' @importFrom DBI dbExecute SQL
#' @importFrom dbplyr sql_render
#' @importFrom dplyr anti_join coalesce count distinct filter if_else inner_join left_join mutate pull select summarise
#' @importFrom purrr walk
#' @importFrom rlang .data

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

  # Vector input: delegate to list method for consistent iteration
  if (length(object) > 1) {
    compute_score(
      object = as.list(object),
      control = control,
      locations = locations
    )
    return(invisible(TRUE))
  }

  .check_batch(object)

  # Exclude locations already computed for this (control, object) combination
  existing_locations <- .get_full(
    table = "data_score",
    in_batch_c = control,
    in_batch_o = object
  )
  loc_remaining <- locations[!(locations %in% existing_locations)]

  if (length(loc_remaining) == 0) {
    message(sprintf(
      "No new locations to compute | control: %s | object: %s.",
      control, object
    ))
    return(invisible(0L))
  }

  # Restrict the raw downloads to remaining locations
  exp_object <- gt.env$tbl_object |>
    filter(
      .data$batch_c == control,
      .data$batch_o == object,
      .data$location %in% loc_remaining
    )

  exp_control <- gt.env$tbl_control |>
    filter(
      .data$batch == control,
      .data$location %in% loc_remaining
    )

  # Fast emptiness check (avoid building long lazy pipelines if no data exists)
  n_obj <- exp_object |>
    count() |>
    collect() |>
    pull(.data$n)

  if (n_obj == 0) {
    message(sprintf(
      "No object data found | control: %s | object: %s.",
      control, object
    ))
    return(invisible(0L))
  }

  # -----------------------------------------------------------------------
  # Benchmark construction
  # -----------------------------------------------------------------------
  # Identify which keywords are control keywords (from the control batch table)
  control_terms <- exp_control |>
    distinct(.data$keyword)

  # Join object/control hits for the *control keywords* and compute a benchmark
  # per (location, date). We guard against zeros by replacing 0 with 1 when
  # forming ratios, mirroring the intent of the original implementation.
  benchmark <- exp_object |>
    inner_join(control_terms, by = "keyword") |>
    inner_join(
      exp_control,
      by = c("location", "keyword", "date"),
      suffix = c("_o", "_c")
    ) |>
    mutate(
      hits_o = if_else(coalesce(.data$hits_o, 0) == 0, 1, .data$hits_o),
      hits_c = if_else(coalesce(.data$hits_c, 0) == 0, 1, .data$hits_c),
      ratio = .data$hits_o / .data$hits_c
    ) |>
    summarise(
      benchmark = mean(.data$ratio, na.rm = TRUE),
      .by = c(.data$location, .data$date)
    )

  # Map control hits to the object scale and compute total mapped control mass
  control_mass <- exp_control |>
    inner_join(benchmark, by = c("location", "date")) |>
    mutate(hits_mapped = .data$hits * coalesce(.data$benchmark, 0)) |>
    summarise(
      hits_c = sum(.data$hits_mapped, na.rm = TRUE),
      .by = c(.data$location, .data$date)
    )

  # Compute scores for object keywords (exclude control keywords)
  out <- exp_object |>
    anti_join(control_terms, by = "keyword") |>
    left_join(control_mass, by = c("location", "date")) |>
    mutate(
      score = if_else(
        is.na(.data$hits_c) | .data$hits_c <= 0,
        0,
        coalesce(.data$hits, 0) / .data$hits_c
      ),
      batch_c = control,
      batch_o = object
    ) |>
    select(
      .data$location,
      .data$keyword,
      .data$date,
      .data$score,
      .data$batch_c,
      .data$batch_o
    )

  # dbplyr appends a trailing semicolon that is invalid inside INSERT ... SELECT
  sql_select <- sub(";\\s*$", "", sql_render(out))

  n_out <- dbExecute(
    gt.env$globaltrends_db,
    SQL(paste0("INSERT INTO data_score ", sql_select))
  )

  message(sprintf(
    "Successfully computed search scores | control: %s | object: %s.",
    control, object
  ))

  invisible(as.integer(n_out))
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(object, control = 1, locations = NULL) {
  args <- .resolve_score_args(control, locations)
  walk(object, compute_score, control = args$control, locations = args$locations)
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

# Validates and resolves the shared `control` and `locations` arguments for
# both S3 methods. Centralised here to avoid the two methods drifting apart.
# @noRd
.resolve_score_args <- function(control, locations) {
  control <- unlist(control)
  .check_length(control, 1)
  .check_batch(control)
  if (is.null(locations)) {
    locations <- if (!is.null(gt.env$countries)) gt.env$countries else globaltrends::countries
  }
  .check_input(locations, "character")
  list(control = control, locations = locations)
}
