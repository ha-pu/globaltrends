#' @title Export changes in data
#'
#' @description
#' The function allows to export changes in search scores, voi, and doi and
#' shows percentile of changes to identify abnormal changes. In combination with
#' various \emph{write} functions in R, the functions allow exports from the
#' database to local files.
#'
#' @details
#' The function computes changes in VOI or DOI between dates. To identify
#' abnormal changes, the function computes the percentile rank for each change
#' within the distribution of changes. Low percentile rank signify abnormally
#' high negative changes. High percentile ranks signify abnormally high positive
#' changes.
#' Exports can be filtered by \emph{keyword}, \emph{object}, \emph{control},
#' \emph{locations}, \emph{type}, or \emph{measure}. Not all filters are
#' applicable for all functions. When filter \emph{keyword} and \emph{object}
#' are used together, \emph{keyword} overrules \emph{object}. Currently the
#' functions do not include list inputs - users are advised to
#' \code{purrr::map_dfr} or \code{dplyr::filter} instead.
#' 
#' @inheritParams export_control
#' @param measure Object of class \code{character} indicating the measure used
#' for DOI computation for which abnormal changes should be analyzed. Takes
#' either \emph{gini}, \emph{hhi}, or \emph{entropy}. Defaults to \emph{"gini"}.
#'
#' @return
#' The functions export and filter the respective database tables and return
#' objects of class \code{"tbl_df", "tbl", "data.frame"}.
#' \itemize{
#'   \item \code{export_voi_change} exports data from table \emph{data_score}
#'   with columns keyword, date, control, object, voi, voi_change, quantile,
#'   filters for \code{location == "world"}.
#'   \item \code{export_doi_change} exports data from table \emph{data_doi} with
#'   columns keyword, date, type, control, object, locations, doi, doi_change,
#'   quantile.
#' }
#' 
#' @seealso
#' * \code{\link{export_doi}}
#' * \code{\link[purrr]{map}}
#' * \code{\link[dplyr]{filter}}
#'
#' @examples
#' \dontrun{
#' export_voi_change(
#'   keyword = "amazon",
#'   type = "obs"
#' )
#' 
#' export_doi_change(
#'  keyword = "amazon",
#'   locations = "countries",
#'   type = "obs",
#'   measure = "gini"
#' )
#' 
#' # interaction with purrr::map_dfr
#' purrr::map_dfr(
#'   c("coca cola", "microsoft"),
#'   export_voi_change,
#'   control = 1,
#'   type = "obs"
#'  )
#' 
#' # interaction with dplyr::filter
#' export_doi_change(
#'   object = 1,
#'   control = 1,
#'   locations = "countries",
#'   type = "obs",
#'   measure = "gini"
#' ) %>%
#'   dplyr::filter(lubridate::year(date) == 2019)
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

export_voi_change <- function(keyword = NULL, object = NULL, control = NULL, type = "obs") {
  .check_type(type)
  
  data <- export_voi(keyword = keyword, object = object, control = control)
  
  data$voi <- data[glue("score_{type}")][[1]]
  data <- group_by(data, .data$keyword, .data$control)
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

#' @rdname export_change
#' @export

export_doi_change <- function(keyword = NULL, object = NULL, control = NULL, locations = NULL, type = NULL, measure = "gini") {
  .check_measure(measure)
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
