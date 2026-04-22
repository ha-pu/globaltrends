#' @title Compute degree of internationalization (DOI)
#'
#' @aliases
#' compute_doi
#' compute_doi.numeric
#' compute_doi.list
#'
#' @description
#' Computes degree of internationalization (DOI) for object keywords based on
#' the cross-location distribution of search scores. DOI is computed per
#' `(keyword, date)` combination for a given control batch (`batch_c`), object
#' batch (`batch_o`), and a named location set (e.g., `"countries"`). Results
#' are appended to the `data_doi` database table.
#'
#' @details
#' DOI captures how evenly search interest is spread across a set of locations:
#' a perfectly uniform score vector yields the maximum DOI, while one
#' concentrated in a single location yields the minimum.
#'
#' Three complementary dispersion measures are computed for each
#' `(keyword, date)` series:
#'
#' \describe{
#'   \item{`gini`}{`1 - Gini(score)`. Uses the rank-weighted formula
#'     `Gini = (2 * sum(score[i] * i) / sum(score) - (n + 1)) / n` over the
#'     sorted score vector. Ranges from 0 (complete concentration) to 1
#'     (perfect equality).}
#'   \item{`hhi`}{`1 - HHI(score)` where `HHI = sum(p^2)` and
#'     `p = score / sum(score)`. Ranges from 0 (monopoly) to `1 - 1/n`
#'     (perfect equality across `n` locations).}
#'   \item{`entropy`}{`H(p) - log(n)` where `p = score / sum(score)`,
#'     `H(p) = -sum(p * log(p))` is Shannon entropy, and `n` is the number
#'     of locations with non-zero scores. Always `<= 0`; equals 0 when scores
#'     are perfectly uniform and becomes more negative as concentration
#'     increases. Zero scores are excluded before computing logs.}
#' }
#'
#' If all scores for a `(keyword, date)` series are `NA`, all three measures
#' are set to `NA`. If all non-`NA` scores are zero, `gini` and `hhi` return
#' 0 and `entropy` returns 0.
#'
#' Score data must already exist in `data_score`, typically produced by
#' [compute_score()]. Only locations whose `type` in `data_locations` matches
#' the `locations` argument are included. The global aggregate
#' (`location == "world"`) is excluded unless the location set explicitly
#' contains it.
#'
#' If DOI for the requested `(batch_c, batch_o, locations)` combination already
#' exists in `data_doi`, the function emits a message and returns early without
#' recomputing.
#'
#' @param object Numeric scalar, vector, or list of numerics. One or more object
#'   batch ids (`batch_o`) identifying keyword groups for which DOI should be
#'   computed. A numeric vector is processed element-by-element (equivalent to
#'   passing a list).
#'
#' @param control Numeric scalar. Control batch id (`batch_c`) identifying the
#'   baseline keyword group used for score normalisation. Defaults to `1`.
#'
#' @param locations Character scalar. Name of a location set stored in
#'   `data_locations$type` (e.g., `"countries"`, `"us_states"`). Only
#'   locations belonging to this set are included in the DOI computation.
#'   Defaults to `"countries"`.
#'
#' @return
#' Invisibly returns the tibble appended to `data_doi` for the processed batch,
#' with columns `date`, `keyword`, `gini`, `hhi`, `entropy`, `batch_c`,
#' `batch_o`, and `locations`. Returns an empty tibble when DOI already exists
#' or when no matching score data is found. Called primarily for its side
#' effects (database writes) and emits a progress message per batch.
#'
#' @seealso [compute_score()] to produce the score data consumed by this
#'   function; `data_doi` for the database table schema.
#'
#' @examples
#' \dontrun{
#' compute_doi(object = 1, control = 1, locations = "countries")
#' compute_doi(object = as.list(1:5), control = 1, locations = "countries")
#' }
#'
#' @export
#' @rdname compute_doi
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr collect distinct filter inner_join mutate select
#' @importFrom purrr map_dbl walk
#' @importFrom rlang .data
#' @importFrom tidyr nest

compute_doi <- function(object, control = 1, locations = "countries") {
  UseMethod("compute_doi", object)
}

#' @rdname compute_doi
#' @method compute_doi numeric
#' @export

compute_doi.numeric <- function(object, control = 1, locations = "countries") {
  control <- unlist(control)
  .check_length(control, 1)
  .check_batch(control)

  .check_length(locations, 1)
  .check_input(locations, "character")

  if (length(object) > 1) {
    return(invisible(compute_doi(
      object = as.list(object),
      control = control,
      locations = locations
    )))
  }

  .check_batch(object)

  if (!.test_empty(batch_c = control, batch_o = object, locations = locations)) {
    message(sprintf(
      "DOI already exists | control: %s | object: %s | locations: %s.",
      control, object, locations
    ))
    return(invisible(tibble()))
  }

  score_df <- gt.env$tbl_locations |>
    filter(.data$type == locations) |>
    distinct(.data$location) |>
    inner_join(gt.env$tbl_score, by = "location") |>
    filter(.data$batch_c == control, .data$batch_o == object) |>
    select(.data$date, .data$keyword, .data$location, .data$score, .data$batch_c) |>
    collect()

  if (nrow(score_df) == 0) {
    message(sprintf(
      "No score data found | control: %s | object: %s | locations: %s.",
      control, object, locations
    ))
    return(invisible(tibble()))
  }

  out <- score_df |>
    tidyr::nest(
      data = c(.data$location, .data$score),
      .by = c(.data$date, .data$keyword, .data$batch_c)
    ) |>
    mutate(
      gini    = map_dbl(.data$data, ~ .compute_gini(.x$score)),
      hhi     = map_dbl(.data$data, ~ .compute_hhi(.x$score)),
      entropy = map_dbl(.data$data, ~ .compute_entropy(.x$score))
    ) |>
    select(
      .data$date,
      .data$keyword,
      .data$gini,
      .data$hhi,
      .data$entropy,
      .data$batch_c
    ) |>
    mutate(
      batch_o   = object,
      locations = locations
    )

  dbAppendTable(
    conn = gt.env$globaltrends_db,
    name = "data_doi",
    value = out
  )

  max_o <- tryCatch(
    max(gt.env$keywords_object$batch, na.rm = TRUE),
    error = function(e) NA_integer_
  )
  suffix <- if (is.finite(max_o)) sprintf(" [%s/%s]", object, max_o) else ""

  message(sprintf(
    "Successfully computed DOI | control: %s | object: %s | locations: %s%s",
    control, object, locations, suffix
  ))

  invisible(out)
}

#' @rdname compute_doi
#' @method compute_doi list
#' @export

compute_doi.list <- function(object, control = 1, locations = "countries") {
  control <- unlist(control)
  .check_length(control, 1)
  .check_batch(control)

  .check_length(locations, 1)
  .check_input(locations, "character")

  walk(object, compute_doi, control = control, locations = locations)
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# Internal DOI metrics
# -------------------------------------------------------------------------

#' @title Inverted Gini coefficient
#'
#' @description
#' Computes `1 - Gini(x)` for a non-negative numeric vector using the
#' rank-weighted formula applied to the sorted input:
#' `Gini = (2 * sum(x[i] * i) / sum(x) - (n + 1)) / n`.
#' Inverting yields a measure where higher values indicate more equal
#' distributions.
#'
#' @param series Numeric vector of non-negative scores, possibly containing
#'   `NA`s.
#'
#' @return A length-1 double: `NA_real_` if all values are `NA`; `0` if total
#'   mass is zero or non-finite; otherwise a value in `[0, 1]`.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr coalesce

.compute_gini <- function(series) {
  x <- series[!is.na(series)]
  if (length(x) == 0L) {
    return(NA_real_)
  }

  s <- sum(x)
  if (!is.finite(s) || s <= 0) {
    return(0)
  }

  x <- sort(x)
  n <- length(x)
  g <- (2 * sum(x * seq_len(n)) / s - (n + 1)) / n
  coalesce(1 - g, 0)
}

#' @title Inverted Herfindahl-Hirschman index (HHI)
#'
#' @description
#' Computes `1 - HHI(x)` where `HHI = sum(p^2)` and `p = x / sum(x)` are
#' location-share weights. The standard HHI measures market concentration;
#' inverting it gives a measure of distributional equality that ranges from
#' `0` (monopoly) to `1 - 1/n` (perfect equality across `n` locations).
#'
#' @param series Numeric vector of non-negative scores, possibly containing
#'   `NA`s.
#'
#' @return A length-1 double: `NA_real_` if all values are `NA`; `0` if total
#'   mass is zero or non-finite; otherwise a value in `[0, 1 - 1/n]` where `n`
#'   is the number of non-`NA` observations.
#'
#' @keywords internal
#' @noRd
#' @importFrom dplyr coalesce

.compute_hhi <- function(series) {
  x <- series[!is.na(series)]
  if (length(x) == 0L) {
    return(NA_real_)
  }

  s <- sum(x)
  if (!is.finite(s) || s <= 0) {
    return(0)
  }

  p <- x / s
  coalesce(1 - sum(p^2), 0)
}

#' @title Entropy-based dispersion measure
#'
#' @description
#' Computes `H(p) - log(n)` where `p = x / sum(x)`,
#' `H(p) = -sum(p * log(p))` is Shannon entropy, and `n` is the number of
#' locations with non-zero scores. This equals the entropy deficit from the
#' theoretical maximum `log(n)` (achieved under perfect uniformity). Zero
#' scores are excluded before computing logs to avoid `log(0)`.
#'
#' The result is always `<= 0`, reaching `0` only when all non-zero scores are
#' equal, and becoming more negative as concentration increases.
#'
#' @param series Numeric vector of non-negative scores, possibly containing
#'   `NA`s.
#'
#' @return A length-1 double: `NA_real_` if all values are `NA`; `0` if all
#'   non-`NA` values are zero, if total mass is non-positive, or if the result
#'   is non-finite; otherwise a value in `(-Inf, 0]`.
#'
#' @keywords internal
#' @noRd

.compute_entropy <- function(series) {
  x <- series[!is.na(series)]
  if (length(x) == 0L) {
    return(NA_real_)
  }

  x <- x[x != 0]
  if (length(x) == 0L) {
    return(0)
  }

  s <- sum(x)
  if (!is.finite(s) || s <= 0) {
    return(0)
  }

  val <- sum(x * log(x / mean(x))) / s
  out <- -val
  if (!is.finite(out)) 0 else out
}
