#' @title Aggregate keyword-country data and compute DOI
#'
#' @aliases
#' compute_doi
#' compute_doi.numeric
#' compute_doi.list
#'
#' @description
#' @details
#'
#' @param control Control batch for which the search score is used. Object
#' of class \code{numeric}.
#' @param object Object batch for which the keyword-country data
#' is aggregated and DOI is computed.  Object of class \code{numeric}.
#' @param locations List of locations for which the search score is used.
#' Object of class \code{character}.
#'
#' @seealso
#'
#' @return
#' Message that data was aggregated successfully. Data is uploaded
#' to data_doi.
#'
#' @examples
#' \dontrun{
#' compute_doi(object = 1, control = 1, locations = "countries")
#' compute_doi(object = as.list(1:5), control = 1, locations = "countries")
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
#' @importFrom purrr walk
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
    control <- control[[1]]
    walk(c(control, object), .test_batch)
    if (.test_empty(table = "data_doi", batch_c = control, batch_o = object, locations = locations)) {
      data <- collect(filter(.tbl_score, batch_c == control & batch_o == object))
      data <- filter(
        data,
        location %in% pull(
          collect(filter(.tbl_locations, type == locations)),
          location
        )
      )
      data <- data[!(data$keyword %in% .keyword_synonyms$synonym), ]

      # compute doi measures
      out <- pivot_longer(data, cols = contains("score"), names_to = "type", values_to = "score")
      out <- nest(out, data = c(location, score))
      out <- mutate(out,
        gini = map_dbl(data, ~ .compute_gini(series = .x$score)),
        hhi = map_dbl(data, ~ .compute_hhi(series = .x$score)),
        entropy = map_dbl(data, ~ .compute_entropy(series = .x$score))
      )
      out <- select(out, date, keyword, type, gini, hhi, entropy)

      # write data
      out <- mutate(out, batch_c = control, batch_o = object, locations = locations)
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

.compute_hhi <- function(series) {
  out <- coalesce(1 - sum((series / sum(series))^2), 0)
  return(out)
}

#' @title Compute entropy
#'
#' @rdname hlprs
#' @keywords internal

.compute_entropy <- function(series) {
  out <- coalesce(-1 * ineq::ineq(series, parameter = 1, type = "entropy"), 0)
  if (out == -Inf) {
    out <- 0
  }
  return(out)
}
