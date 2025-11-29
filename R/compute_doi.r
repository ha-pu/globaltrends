#' @title Aggregate keyword-country data and compute DOI
#'
#' @aliases
#' compute_doi
#' compute_doi.numeric
#' compute_doi.list
#'
#' @description
#' The function computes degree of internationalization (DOI) for object
#' keywords. Degree of internationalization is measured based on the
#' distribution of country search scores.
#'
#' @details
#' The function uses an inverted Gini-coefficient as measure for the degree of
#' internationalization. The more uniform the distribution of search scores
#' across all countries, the higher the inverted Gini-coefficient and the
#' greater the degree of internationalization. In addition to the
#' Gini-coefficient, the package uses inverted Herfindahl index and inverted
#' Entropy as measures for internationalization.
#'
#' @param control Control batch for which the search score is used. Object
#' of type `numeric`.
#'
#' @param object Object batch for which the keyword-country data
#' is aggregated and DOI is computed.  Object of type `numeric`.
#'
#' @param locations List of locations for which the search score is used.
#' Object of type `character`. Defaults to *"countries"*.
#'
#' @seealso
#' * [example_doi()]
#'
#' @return
#' Message that data was aggregated successfully. Data is written to table
#' *data_doi*.
#'
#' @examples
#' \dontrun{
#' compute_doi(
#'   object = 1,
#'   control = 1,
#'   locations = "countries"
#' )
#' compute_doi(
#'   object = as.list(1:5),
#'   control = 1,
#'   locations = "countries"
#' )
#' }
#'
#' @export
#' @rdname compute_doi
#' @importFrom DBI dbAppendTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer

compute_doi <- function(object, control = 1, locations = "countries") {
  UseMethod("compute_doi", object)
}

#' @rdname compute_doi
#' @method compute_doi numeric
#' @export

compute_doi.numeric <- function(object, control = 1, locations = "countries") {
  control <- unlist(control)
  .check_length(control, 1)
  .check_length(locations, 1)
  .check_input(locations, "character")
  if (length(object) > 1) {
    compute_doi(
      control = control,
      object = as.list(object),
      locations = locations
    )
  } else {
    walk(list(control, object), .check_batch)
    if (
      .test_empty(
        batch_c = control,
        batch_o = object,
        locations = locations
      )
    ) {
      data <- gt.env$tbl_locations |>
        filter(.data$type == locations) |>
        distinct() |>
        inner_join(gt.env$tbl_score, by = "location") |>
        filter(
          .data$batch_c == control & .data$batch_o == object
        ) |>
        collect() |>
        nest(data = c(location, score)) |>
        mutate(check = map_lgl(data, ~ !all(is.na(.x$score))))

      # compute doi measures
      out1 <- data |>
        filter(.data$check) |>
        mutate(
          gini = map_dbl(data, ~ .compute_gini(series = .x$score)),
          hhi = map_dbl(data, ~ .compute_hhi(series = .x$score)),
          entropy = map_dbl(data, ~ .compute_entropy(series = .x$score))
        )

      out2 <- data |>
        filter(!.data$check) |>
        mutate(
          gini = NA,
          hhi = NA,
          entropy = NA
        )

      # write data
      out <- out1 |>
        bind_rows(out2) |>
        select(
          date,
          keyword,
          gini,
          hhi,
          entropy
        ) |>
        mutate(
          batch_c = control,
          batch_o = object,
          locations = locations
        )

      dbAppendTable(
        conn = gt.env$globaltrends_db,
        name = "data_doi",
        value = out
      )
    }
    message(paste0(
      "Successfully computed DOI | control: ",
      control,
      " | object: ",
      object,
      " [",
      object,
      "/",
      max(gt.env$keywords_object$batch),
      "]"
    ))
  }
  dbExecute(
    gt.env$globaltrends_db,
    "COPY data_doi TO 'db/data_doi.parquet' (FORMAT parquet);"
  )
}

#' @rdname compute_doi
#' @method compute_doi list
#' @export

compute_doi.list <- function(object, control = 1, locations = "countries") {
  walk(object, compute_doi, control = control, locations = locations)
}

#' @title Compute gini coefficient
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr coalesce

.compute_gini <- function(series) {
  gini <- function(x) {
    n <- length(x)
    x <- sort(x)
    g <- sum(x * seq(n))
    g <- 2 * g / sum(x) - (n + 1)
    g <- g / n
    return(g)
  }

  out <- coalesce(1 - gini(series), 0)
  return(out)
}

#' @title Compute herfindahl hirschman index
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd

.compute_hhi <- function(series) {
  out <- coalesce(1 - sum((series / sum(series))^2), 0)
  return(out)
}

#' @title Compute entropy
#'
#' @rdname hlprs
#' @keywords internal
#' @noRd

.compute_entropy <- function(series) {
  entropy <- function(x) {
    x <- x[!(x == 0)]
    e <- x / mean(x)
    e <- sum(x * log(e))
    e <- e / sum(x)
    return(e)
  }

  out <- coalesce(-1 * entropy(series), 0)
  if (out == -Inf) {
    out <- 0
  }
  return(out)
}
