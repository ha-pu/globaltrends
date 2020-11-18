#' @title Plot doiGT data
#' 
#' @description 
#' @details 
#' 
#' @param data_doi Data exported from \code{export_doi} function.
#' @param type Object of class \code{character} indicating the type of time
#' series-column from data_score that is used for DOI computation, takes
#' either "obs", "sad", or "trd". By default takes \code{NULL}
#' and assumes that only one single type of timeseries is included in the
#' data.
#' @param measure Object of class \code{character} indicating the measure
#' used for DOI computation, takes either "gini", "hhi", or "entropy".
#' Defaults to "gini".
#' @param locations Object of class \code{character} indicating for which
#' set of locations should be filtered. By default takes \code{NULL} and
#' assumes that only one single set of locations is included in the data.
#' @param grid Object of class \code{logical} indicating whether the
#' \code{facet_wrap} function of \code{ggplot2} should be used. Otherwise,
#' keywords are separated by color.
#' @param smooth Object of class \code{logical} indicating whether the
#' \code{geom_smooth} function of \code{ggplot2} should be used. Only
#' applicale when \code{grid == TRUE}.
#' 
#' @return Plot of timeseries as \code{ggplot2} object
#' 
#' @examples
#' \dontrun{
#' data <- export_doi(object = 1, locations = "countries")
#' plot_box(data_doi = data, type = "score_obs", measure = "gini")
#' plot_box(data_doi = data, type = "score_sad", measure = "hhi")
#' plot_box(data_doi = data, type = "score_trd", measure = "entropy")
#' }
#' 
#' @rdname plot
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom stringr str_detect

plot_box <- function(data_doi, type = NULL, measure = "gini", locations = NULL) {
  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a boxplot.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_doi <- filter(data_doi, keyword %in% unique(data_doi$keyword)[1:9])
  }
  data_doi$measure <- data_doi[measure][[1]]
  if (!is.null(in_type)) data_doi <- filter(data_doi, str_detect(type, in_type))
  if (!is.null(in_locations)) data_doi <- filter(data_doi, locations == in_locations)
  plot <- ggplot(data_doi, aes(x = keyword, y = measure)) +
    geom_boxplot() +
    labs(x = NULL, y = "Degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))
  
  return(plot)
}

#' @examples
#' \dontrun{
#' data <- export_doi(object = 1, locations = "countries")
#' plot_ts(data_doi = data, type = "score_obs", measure = "gini", grid = TRUE, smooth = TRUE)
#' plot_ts(data_doi = data, type = "score_sad", measure = "hhi", grid = TRUE, smooth = FALSE)
#' plot_ts(data_doi = data, type = "score_trd", measure = "entropy", grid = FALSE, smooth = TRUE)
#' }
#' 
#' @rdname plot
#' @export
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 facet_wrap

plot_ts <- function(data_doi, type = NULL, measure = "gini", locations = NULL, grid = TRUE, smooth = TRUE) {
  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  data_doi$measure <- data_doi[measure][[1]]
  if (!is.null(in_type)) data_doi <- filter(data_doi, str_detect(type, in_type))
  if (!is.null(in_locations)) data_doi <- filter(data_doi, locations == in_locations)
  plot <- ggplot(data_doi, aes(x = date))
  
  if (grid) {
    if (len_keywords > 9) {
      warning(glue("The plot function is limited to 9 keywords in a grid.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
      data_doi <- filter(data_doi, keyword %in% unique(data$keyword)[1:9])
    }
    plot <- plot +
      geom_line(aes(y = measure)) +
      facet_wrap(~keyword)
    
    if (smooth) {
      plot <- plot +
        geom_smooth(aes(y = measure))
    }
  } else {
    if (len_keywords > 9) {
      warning(glue("The plot function is limited to 5 keywords as coloured lines.\nYou use {len_keywords} keywords.\nOnly the first 5 keywords are used."))
      data_doi <- filter(data_doi, keyword %in% unique(data$keyword)[1:5])
    }
    plot <- plot +
      geom_line(aes(y = measure, colour = keyword))
    
    if (smooth) {
      warning("The smoothing option currently only works for 'grid == TRUE'.")
    }
  }
  
  plot <- plot +
    labs(x = NULL, y = "Degree of internationalization", colour = "Keyword", caption = glue("DOI computed as {str_to_upper(measure)}.")) +
    theme(legend.position = "bottom")
  
  return(plot)
}
