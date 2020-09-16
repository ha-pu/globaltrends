#' @title Plot doiGT boxplots
#' 
#' @description 
#' @details 
#' 
#' @param data Data exported from \code{export_doi} function.
#' @param type Object of class \code{character} indicating the type of time
#' series-column from data_score that is used for DOI computation, takes
#' either "socre_obs", "score_sad", or "score_trd". By default takes \code{NULL}
#' and assumes that only one single type of timeseries is included in the
#' data.
#' @param measure Object of class \code{character} indicating the measure
#' used for DOI computation, takes either "gini", "hhi", or "entropy".
#' Defaults to "gini".
#' @param locations Object of class \code{character} indicating for which
#' set of locations should be filtered. By default takes \code{NULL} and
#' assumes that only one single set of locations is included in the data.
#' 
#' @return Boxplot of timeseries as \code{ggplot2} object
#' 
#' @examples
#' \dontrun{
#' data <- export_doi(object = 1, locations = "countries")
#' plot_box(data, type = "score_obs", measure = "gini")
#' plot_box(data, type = "score_sad", measure = "hhi")
#' plot_box(data, type = "score_trd", measure = "entropy")
#' }
#' 
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom glue glue

plot_box <- function(data, type = NULL, measure = "gini", locations = NULL) {
  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a boxplot.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data[measure][[1]]
  if (!is.null(in_type)) data <- filter(data, type == in_type)
  if (!is.null(in_locations)) data <- filter(data, locations == in_locations)
  plot <- ggplot(data, aes(x = keyword, y = measure)) +
    geom_boxplot() +
    labs(x = NULL, y = "Degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))
  
  return(plot)
}
