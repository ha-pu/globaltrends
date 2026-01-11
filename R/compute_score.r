#' @title Compute search scores for object keywords
#'
#' @aliases
#' compute_score
#' compute_score.numeric
#' compute_score.list
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
#' Operationally, for each requested object batch (`batch_o`) and control batch
#' (`batch_c`), the function:
#' \enumerate{
#'   \item Identifies the subset of `locations` that have not yet been computed
#'   for `(batch_c, batch_o)` in `data_score`.
#'   \item Computes a per-`(location, date)` *benchmark* as the mean ratio of
#'   object/control hits **for the control keywords** that are present in both
#'   downloads.
#'   \item Maps the control batch to the object scale via `hits_mapped = hits * benchmark`.
#'   \item Sums mapped control hits across control keywords to obtain `hits_c`
#'   and computes `score = hits_object / hits_c` for each object keyword.
#'   \item Inserts the resulting rows into `data_score`.
#' }
#'
#' If synonym keywords were specified via [add_synonym()], you should run
#' [aggregate_synonyms()] after score computation to roll synonym scores into
#' their canonical terms.
#'
#' References:
#' Castelnuovo, E. & Tran, T. D. (2017). *Google It Up! A Google Trends-based
#' Uncertainty index for the United States and Australia.* Economics Letters, 161, 149-153.
#'
#' @param object Numeric scalar/vector (or list of numeric scalars). Object batch
#'   id(s) (`batch_o`) for which scores should be computed.
#'
#' @param control Numeric scalar. Control batch id (`batch_c`) used as baseline.
#'   Defaults to `1`.
#'
#' @param locations Character vector of location codes (e.g., `countries`,
#'   `us_states`) or `"world"` for global VOI-style computation. If `NULL`,
#'   defaults to `gt.env$countries` when available, otherwise `globaltrends::countries`.
#'
#' @return
#' Invisibly returns `TRUE` (list method) or the number of inserted rows
#' (numeric method, best-effort) for the processed batch. The function is
#' called primarily for its side effects (writing to `data_score`) and emits
#' a message per batch.
#'
#' @examples
#' \dontrun{
#' compute_score(object = 1, control = 1, locations = countries)
#' compute_score(object = as.list(1:5), control = 1, locations = countries)
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
    message(paste0(
      "No new locations to compute | control: ",
      control,
      " | object: ",
      object,
      "."
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
    message(paste0(
      "No object data found | control: ",
      control,
      " | object: ",
      object,
      "."
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

  # Insert lazily computed results via INSERT ... SELECT
  n_out <- out |>
    count() |>
    collect() |>
    pull(.data$n)

  if (n_out > 0) {
    sql_select <- sql_render(out)
    sql_select <- sub(";\\s*$", "", sql_select)

    dbExecute(
      gt.env$globaltrends_db,
      SQL(paste0("INSERT INTO data_score ", sql_select))
    )
  }

  message(paste0(
    "Successfully computed search scores | control: ",
    control,
    " | object: ",
    object,
    "."
  ))

  invisible(as.integer(n_out))
}

#' @rdname compute_score
#' @method compute_score list
#' @export

compute_score.list <- function(object, control = 1, locations = NULL) {
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

  walk(object, compute_score, control = control, locations = locations)
  invisible(TRUE)
}

#' @title Compute volume of internationalization (VOI)
#'
#' @description
#' Convenience wrapper around [compute_score()] that computes scores for the
#' global aggregate only (`location == "world"`).
#'
#' @param object Numeric scalar/vector (or list) of object batch id(s).
#' @param control Numeric scalar. Control batch id. Defaults to `1`.
#'
#' @return Invisibly returns `TRUE` (or a batch-wise insert count) via
#' [compute_score()].
#'
#' @export
#' @rdname compute_score

compute_voi <- function(object, control = 1) {
  compute_score(object = object, control = control, locations = "world")
}
