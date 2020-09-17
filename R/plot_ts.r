#' @title Line plot of data_doi timeseries
#'
#' @description
#' @details
#'
#' @inheritParams plot_box
#' @param grid Object of class \code{logical} indicating whether the
#' \code{facet_wrap} function of \code{ggplot2} should be used. Otherwise,
#' keywords are separated by color.
#' @param smooth Object of class \code{logical} indicating whether the
#' \code{geom_smooth} function of \code{ggplot2} should be used. Only
#' applicale when \code{grid == TRUE}.
#'
#' @section Warning:
#' \code{plot_ts} is limited to 9 unique keywords for \code{grid == TRUE}
#' and 5 unique keywords otherwise to avoid an overcrowded plot. When
#' \code{data_doi} includes more than 9 or 5 unique keywords, only
#' the first 9 or 5 keywords are used.
#'
#' @return Line plot of timeseries as \code{ggplot2} object
#'
#' @examples
#' \dontrun{
#' data <- export_doi(object = 1, locations = "countries")
#' plot_ts(data_doi = data, type = "obs", measure = "gini", grid = TRUE, smooth = TRUE)
#' plot_ts(data_doi = data, type = "sad", measure = "hhi", grid = TRUE, smooth = FALSE)
#' plot_ts(data_doi = data, type = "trd", measure = "entropy", grid = FALSE, smooth = TRUE)
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

plot_ts <- function(data_doi, type = NULL, measure = "gini", locations = NULL, grid = TRUE, smooth = TRUE) {
  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  data_doi$measure <- data_doi[measure][[1]]
  if (!is.null(in_type)) data_doi <- filter(data_doi, type == paste0("score_", in_type))
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
