#' @title Line plot of data_doi time series
#'
#' @description
#' @details
#'
#' @inheritParams plot_box
#' @param smooth Object of class \code{logical} indicating whether the
#' \code{geom_smooth} function of \code{ggplot2} should be used.
#'
#' @section Warning:
#' \code{plot_ts} is limited to 9 unique keywords to avoid an overcrowded
#'  plot. When \code{data_doi} includes more than 9 unique keywords, only
#' the first 9 keywords are used.
#'
#' @return Line plot of time series as \code{ggplot2} object
#'
#' @examples
#' \dontrun{
#' data <- export_doi(object = 1, locations = "countries")
#' plot_ts(data_doi = data, type = "obs", measure = "gini", smooth = TRUE)
#' plot_ts(data_doi = data, type = "sad", measure = "hhi", smooth = FALSE)
#' plot_ts(data_doi = data, type = "trd", measure = "entropy", smooth = TRUE)
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom stringr str_to_upper

plot_ts <- function(data_doi, type = NULL, measure = "gini", locations = NULL, smooth = TRUE) {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_doi)}."))
  if (!is.null(type) & !(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
  if (!is.null(measure) & !(measure %in% c("gini", "hhi", "entropy"))) stop(glue("Error: 'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou supplied {measure}."))
  if (!is.null(locations) & !is.character(locations)) stop(glue("Error: 'locations' must be of type 'character'.\nYou supplied an object of type {typeof(locations)}."))
  if (!is.null(smooth) & !is.logical(smooth)) stop(glue("Error: 'smooth' must be of type 'logical'.\nYou supplied an object of type {typeof(smooth)}."))

  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  data_doi$measure <- data_doi[measure][[1]]
  if (!is.null(in_type)) data_doi <- filter(data_doi, type == paste0("score_", in_type))
  if (!is.null(in_locations)) data_doi <- filter(data_doi, locations == in_locations)
  plot <- ggplot(data_doi, aes(x = date))


  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a grid.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_doi <- filter(data_doi, keyword %in% unique(data_doi$keyword)[1:9])
  }
  plot <- plot +
    geom_line(aes(y = measure)) +
    facet_wrap(~keyword)

  if (smooth) {
    plot <- plot +
      geom_smooth(aes(y = measure))
  }

  plot <- plot +
    labs(x = NULL, y = "Degree of internationalization", colour = "Keyword", caption = glue("DOI computed as {str_to_upper(measure)}."))

  return(plot)
}
