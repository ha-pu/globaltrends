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
#' The function uses an inverted Gini-coefficient
#' [\code{dplyr::coalesce(1 - ineq::ineq(series, type = "Gini"), 0)}]
#' as measure for the degree of internationalization. The more uniform the
#' distribution of search scores across all countries, the higher the inverted
#' Gini-coefficient and the greater the degree of internationalization. In
#' addition to the Gini-coefficient, the package uses inverted Herfindahl index
#' [\code{coalesce(1 - sum((series / sum(series))^2), 0)}] and inverted Entropy
#' [\code{dplyr::coalesce(-1 * ineq::ineq(series, parameter = 1, type = "entropy"), 0)}]
#' as measures for internationalization.
#'
#' @param control Control batch for which the search score is used. Object
#' of class \code{numeric}.
#' @param object Object batch for which the keyword-country data
#' is aggregated and DOI is computed.  Object of class \code{numeric}.
#' @param locations List of locations for which the search score is used.
#' Object of class \code{character}. Defaults to \emph{"countries"}.
#'
#' @seealso
#' * \code{\link{data_doi}}
#' * \code{\link[ineq]{ineq}}
#'
#' @return
#' Message that data was aggregated successfully. Data is written to table
#' \emph{data_doi}.
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
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer

compute_doi <- function(object, control = 1, locations = "countries") UseMethod("compute_doi", object)

#' @rdname compute_doi
#' @method compute_doi numeric
#' @export

compute_doi.numeric <- function(object, control = 1, locations = "countries") {
  if (length(control) > 1) stop(glue("Error: 'control' must be object of length 1.\nYou provided an object of length {length(control)}."))
  if (length(locations) != 1) stop(glue("Error: Length object 'locations' must not exeed 1.\nYou provided an object with length {length(locations)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be object of type character.\nYou provided an object of type {typeof(locations)}."))
  if (length(object) > 1) {
    compute_doi(control = control, object = as.list(object), locations = locations)
  } else {
    walk(list(control, object), .check_batch)
    if (.test_empty(table = "data_doi", batch_c = control, batch_o = object, locations = locations)) {
      data <- collect(filter(.tbl_score, .data$batch_c == control & .data$batch_o == object))
      data <- filter(
        data,
        .data$location %in% pull(
          collect(filter(.tbl_locations, .data$type == locations)),
          .data$location
        )
      )
      data <- data[!(data$keyword %in% .keyword_synonyms$synonym), ]

      # compute doi measures
      data <- pivot_longer(data, cols = contains("score"), names_to = "type", values_to = "score")
      data <- nest(data, data = c(.data$location, .data$score))
      data <- mutate(data, check = map_lgl(.data$data, ~ !all(is.na(.x$score))))
      out1 <- filter(data, .data$check)
      out1 <- mutate(
        out1,
        gini = map_dbl(.data$data, ~ .compute_gini(series = .x$score)),
        hhi = map_dbl(.data$data, ~ .compute_hhi(series = .x$score)),
        entropy = map_dbl(.data$data, ~ .compute_entropy(series = .x$score))
      )
      out2 <- filter(data, !.data$check)
      out2 <- mutate(
        out2,
        gini = NA,
        hhi = NA,
        entropy = NA
      )
      out <- bind_rows(out1, out2)
      out <- select(
        out,
        .data$date,
        .data$keyword,
        .data$type,
        .data$gini,
        .data$hhi,
        .data$entropy
      )

      # write data
      out <- mutate(
        out,
        batch_c = control,
        batch_o = object,
        locations = locations
      )
      dbWriteTable(conn = globaltrends_db, name = "data_doi", value = out, append = TRUE)
    }
    message(glue("Successfully computed DOI | control: {control} | object: {object} [{object}/{total}]", total = max(.keywords_object$batch)))
  }
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
  out <- coalesce(1 - ineq::ineq(series, type = "Gini"), 0)
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
  out <- coalesce(-1 * ineq::ineq(series, parameter = 1, type = "entropy"), 0)
  if (out == -Inf) {
    out <- 0
  }
  return(out)
}
