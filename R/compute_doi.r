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
#' `(keyword, date)` for a given control batch (`batch_c`), object batch
#' (`batch_o`), and a named location set (e.g., `"countries"`).
#'
#' @details
#' DOI is derived from the dispersion of search scores across locations.
#' Intuitively, the more uniformly distributed the scores are across the chosen
#' location set, the higher the DOI.
#'
#' This implementation writes three inverted concentration/inequality measures:
#' \itemize{
#'   \item `gini`: `1 - Gini(score)`
#'   \item `hhi`: `1 - sum(p^2)` where `p = score / sum(score)` (Herfindahl-Hirschman)
#'   \item `entropy`: normalized negative entropy-like measure (see `.compute_entropy()`)
#' }
#'
#' The function expects that score data is already available in `data_score`,
#' typically produced by [compute_score()]. Only locations present in the named
#' location set are used. Global (`location == "world"`) is not used unless the
#' location set explicitly contains `"world"`.
#'
#' @param object Numeric scalar (or vector) or list of numeric scalars.
#'   Object batch id(s) (`batch_o`) for which DOI should be computed.
#'
#' @param control Numeric scalar. Control batch id (`batch_c`) used as baseline.
#'   Defaults to `1`.
#'
#' @param locations Character scalar. Name of the location set stored in
#'   `data_locations$type` (e.g., `"countries"`, `"us_states"`). Defaults to
#'   `"countries"`.
#'
#' @return
#' Invisibly returns the tibble written to `data_doi` for the processed batch.
#' Called primarily for its side effects (database writes) and emits a progress
#' message per batch.
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
#' @importFrom dplyr bind_rows collect distinct filter inner_join mutate select summarise
#' @importFrom purrr map_dbl map_lgl walk
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

  # Vector input: delegate to list method for consistent iteration semantics
  if (length(object) > 1) {
    return(invisible(compute_doi(
      object = as.list(object),
      control = control,
      locations = locations
    )))
  }

  .check_batch(object)

  # Skip work if DOI already exists (expected to be implemented in .test_empty)
  if (
    !.test_empty(batch_c = control, batch_o = object, locations = locations)
  ) {
    message(paste0(
      "DOI already exists | control: ",
      control,
      " | object: ",
      object,
      " | locations: ",
      locations,
      "."
    ))
    return(invisible(tibble()))
  }

  # -----------------------------------------------------------------------
  # Pull score data for the requested location set and batch combination.
  # We join `data_locations` to restrict to the desired location set.
  # -----------------------------------------------------------------------
  score_df <- gt.env$tbl_locations |>
    filter(.data$type == locations) |>
    distinct(.data$location) |>
    inner_join(gt.env$tbl_score, by = "location") |>
    filter(.data$batch_c == control, .data$batch_o == object) |>
    collect()

  if (nrow(score_df) == 0) {
    message(paste0(
      "No score data found | control: ",
      control,
      " | object: ",
      object,
      " | locations: ",
      locations,
      "."
    ))
    return(invisible(tibble()))
  }

  # -----------------------------------------------------------------------
  # Compute DOI measures per (keyword, date).
  # We nest the location-score series and compute metrics over the score vector.
  # If all scores are NA for a series, DOI measures are set to NA.
  # -----------------------------------------------------------------------
  nested <- score_df |>
    select(
      .data$date,
      .data$keyword,
      .data$location,
      .data$score,
      .data$batch_c
    ) |>
    tidyr::nest(
      data = c(.data$location, .data$score),
      .by = c(.data$date, .data$keyword, .data$batch_c)
    ) |>
    mutate(has_non_na = map_lgl(.data$data, ~ !all(is.na(.x$score))))

  out_ok <- nested |>
    filter(.data$has_non_na) |>
    mutate(
      gini = map_dbl(.data$data, ~ .compute_gini(.x$score)),
      hhi = map_dbl(.data$data, ~ .compute_hhi(.x$score)),
      entropy = map_dbl(.data$data, ~ .compute_entropy(.x$score))
    )

  out_na <- nested |>
    filter(!.data$has_non_na) |>
    mutate(gini = NA_real_, hhi = NA_real_, entropy = NA_real_)

  out <- bind_rows(out_ok, out_na) |>
    select(
      .data$date,
      .data$keyword,
      .data$gini,
      .data$hhi,
      .data$entropy,
      .data$batch_c
    ) |>
    mutate(
      batch_o = object,
      locations = locations
    )

  dbAppendTable(
    conn = gt.env$globaltrends_db,
    name = "data_doi",
    value = out
  )

  # Progress message: avoid referencing gt.env$keywords_object if not initialized
  max_o <- tryCatch(
    max(gt.env$keywords_object$batch, na.rm = TRUE),
    error = function(e) NA_integer_
  )
  suffix <- if (is.finite(max_o)) paste0(" [", object, "/", max_o, "]") else ""

  message(paste0(
    "Successfully computed DOI | control: ",
    control,
    " | object: ",
    object,
    " | locations: ",
    locations,
    suffix
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
#' @description
#' Computes `1 - Gini(x)` for a non-negative numeric vector `x`. Returns `0`
#' when the series is all `NA`, all zeros, or otherwise not computable.
#' @keywords internal
#' @noRd
#' @importFrom dplyr coalesce

.compute_gini <- function(series) {
  x <- series
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(0)
  }

  # If there is no mass, treat as non-informative.
  s <- sum(x)
  if (!is.finite(s) || s <= 0) {
    return(0)
  }

  # Standard Gini for non-negative values
  x <- sort(x)
  n <- length(x)
  g <- sum(x * seq_len(n))
  g <- (2 * g / s - (n + 1)) / n

  coalesce(1 - g, 0)
}

#' @title Inverted Herfindahl-Hirschman index (HHI)
#' @description
#' Computes `1 - sum(p^2)` where `p = x / sum(x)`. Returns `0` when the series
#' is all `NA`, all zeros, or not computable.
#' @keywords internal
#' @noRd
#' @importFrom dplyr coalesce

.compute_hhi <- function(series) {
  x <- series
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(0)
  }

  s <- sum(x)
  if (!is.finite(s) || s <= 0) {
    return(0)
  }

  p <- x / s
  coalesce(1 - sum(p^2), 0)
}

#' @title Inverted entropy-like dispersion measure
#' @description
#' Computes an inverted entropy-like measure used by the package to quantify
#' dispersion. Zero scores are removed before computing logs. Returns `0` for
#' degenerate or non-computable inputs.
#' @keywords internal
#' @noRd
#' @importFrom dplyr coalesce

.compute_entropy <- function(series) {
  x <- series
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(0)
  }

  # Remove zeros to avoid log(0) while preserving meaning
  x <- x[x != 0]
  if (length(x) == 0L) {
    return(0)
  }

  s <- sum(x)
  if (!is.finite(s) || s <= 0) {
    return(0)
  }

  # Original formulation preserved; guarded for numerical issues
  e <- x / mean(x)
  val <- sum(x * log(e)) / s
  out <- coalesce(-1 * val, 0)

  if (!is.finite(out)) {
    out <- 0
  }
  out
}
