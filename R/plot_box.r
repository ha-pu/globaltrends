#' @title Boxplot of data_doi time series
#'
#' @description
#' @details
#'
#' @param data_doi Data exported from \code{export_doi} function.
#' @param type Object of class \code{character} indicating the type of time
#' series-column from data_score that is used for DOI computation, takes
#' either "obs", "sad", or "trd". Defaults to \code{"obs"}.
#' @param measure Object of class \code{character} indicating the measure
#' used for DOI computation, takes either "gini", "hhi", or "entropy".
#' Defaults to "gini".
#' @param locations Object of class \code{character} indicating for which
#' set of locations should be filtered. Defaults to  \code{"countries"}.
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

plot_box <- function(data_doi, type = "obs", measure = "gini", locations = "countries") {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_doi)}."))
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
  if (length(measure) > 1) stop(glue("Error: 'measure' must be object of length 1.\nYou provided an object of length {length(measure)}."))
  if (!(measure %in% c("gini", "hhi", "entropy"))) stop(glue("Error: 'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou supplied {measure}."))
  if (length(locations) > 1) stop(glue("Error: 'locations' must be object of length 1.\nYou provided an object of length {length(locations)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be of type 'character'.\nYou supplied an object of type {typeof(locations)}."))

  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a boxplot.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_doi <- filter(data_doi, keyword %in% unique(data_doi$keyword)[1:9])
  }
  data_doi$measure <- data_doi[measure][[1]]
  data_doi <- filter(data_doi, type == paste0("score_", in_type))
  data_doi <- filter(data_doi, locations == in_locations)
  plot <- ggplot(data_doi, aes(x = keyword, y = measure)) +
    geom_boxplot() +
    labs(x = NULL, y = "Degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))

  return(plot)
}
