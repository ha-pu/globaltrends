#' @title Export changes in data
#'
#' @description
#' The function allows to export changes in search scores, voi, and doi and
#' shows percentile of changes to identify abnormal changes.
#'
#' @details
#'
#' @inheritParams export_control
#' @param measure Object of class \code{character} indicating the measure used
#' for DOI computation for which abnormal changes should be analyzed. Takes
#' either \emph{gini}, \emph{hhi}, or \emph{entropy}. Defaults to \emph{"gini"}.
#'
#' @return
#' 
#' @seealso
#' * \code{\link{export_doi}}
#' * \code{\link[purrr]{map}}
#' * \code{\link[dplyr]{filter}}
#'
#' @examples
#' \dontrun{
#' export_doi_change(keyword = "amazon", type = "obs", locations = "countries")
#' }
#'
#' @rdname export_change
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr percent_rank
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
 
export_doi_change <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL, measure = "gini") {
  # check measure
  data <- export_doi(keyword = keyword, object = object, control = control, locations = locations, type = type)
  data$doi <- data[measure][[1]]
  data <- group_by(data, .data$keyword, .data$type, .data$control, .data$locations)
  data <- mutate(data, doi_change = .data$doi - lag(.data$doi))
  data <- mutate(data, quantile = percent_rank(.data$doi_change))
  data <- ungroup(data)
  data <- select(
    data, 
    .data$keyword, 
    .data$date, 
    .data$type, 
    .data$control, 
    .data$object, 
    .data$locations, 
    .data$doi, 
    .data$doi_change, 
    .data$quantile
  )
  return(data)
}

#' @rdname export_change
#' @export

export_voi_change <- function(keyword = NULL, object = NULL, control = NULL, type = "obs") {
  # check measure
  data <- export_voi(keyword = keyword, object = object, control = control)
  data$voi <- data[glue("score_{type}")][[1]]
  data <- group_by(data, .data$keyword, .data$type, .data$control, .data$locations)
  data <- mutate(data, voi_change = .data$voi - lag(.data$voi))
  data <- mutate(data, quantile = percent_rank(.data$voi_change))
  data <- ungroup(data)
  data <- select(
    data, 
    .data$keyword, 
    .data$date, 
    .data$control, 
    .data$object, 
    .data$voi, 
    .data$voi_change, 
    .data$quantile
  )
  return(data)
}
