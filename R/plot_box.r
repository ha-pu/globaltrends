#' @title Boxplot of data_doi time series
#'
#' @description
#' @details
#'
#' @param data_doi Data exported from \code{export_doi} function.
#' @param type Object of class \code{character} indicating the type of time
#' series-column from data_score that is used for DOI computation, takes
#' either "obs", "sad", or "trd". By default takes \code{NULL}
#' and assumes that only one single type of time series is included in the
#' data.
#' @param measure Object of class \code{character} indicating the measure
#' used for DOI computation, takes either "gini", "hhi", or "entropy".
#' Defaults to "gini".
#' @param locations Object of class \code{character} indicating for which
#' set of locations should be filtered. By default takes \code{NULL} and
#' assumes that only one single set of locations is included in the data.
#'
#' @section Warning:
#' \code{plot_box} is limited to 9 unique keywords to avoid an overcrowded
#' plot. When \code{data_doi} includes more than 9 unique keywords, only
#' the first 9 keywords are used.
#'
#' @return Boxplot of time series as \code{ggplot2} object
#'
#' @examples
#' \dontrun{
#' data <- export_doi(object = 1, locations = "countries")
#' plot_box(data_doi = data, type = "obs", measure = "gini")
#' plot_box(data_doi = data, type = "sad", measure = "hhi")
#' plot_box(data_doi = data, type = "trd", measure = "entropy")
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom stringr str_to_upper

plot_box <- function(data_doi, type = NULL, measure = "gini", locations = NULL) {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_doi)}."))
  if (!is.null(type)) if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
  if (!is.null(measure)) if (!(measure %in% c("gini", "hhi", "entropy"))) stop(glue("Error: 'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou supplied {measure}."))
  if (!is.null(locations) & !is.character(locations)) stop(glue("Error: 'locations' must be of type 'character'.\nYou supplied an object of type {typeof(locations)}."))

  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a boxplot.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_doi <- filter(data_doi, keyword %in% unique(data_doi$keyword)[1:9])
  }
  data_doi$measure <- data_doi[measure][[1]]
  if (!is.null(in_type)) data_doi <- filter(data_doi, type == paste0("score_", in_type))
  if (!is.null(in_locations)) data_doi <- filter(data_doi, locations == in_locations)
  plot <- ggplot(data_doi, aes(x = keyword, y = measure)) +
    geom_boxplot() +
    labs(x = NULL, y = "Degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))

  return(plot)
}
