#' @title Line plot of data_doi and data global time series
#'
#' @description
#' @details
#'
#' @inheritParams plot_box
#' @inheritParams plot_ts
#' @param data_score_global Data exported from \code{export_score_global}
#' function.
#'
#' @section Warning:
#' \code{plot_trend} is limited to 1 unique keyword to avoid an
#' overcrowded plot. When \code{data_doi} or \code{data_score} includes
#' more than 1 unique keyword, only the first keyword is used.
#'
#' @return Line plot of time series as \code{ggplot2} object
#'
#' @examples
#' \dontrun{
#' data1 <- export_doi(keyword = "manchester united", locations = "countries")
#' data2 <- export_score_global(keyword = "manchester united")
#' plot_trend(data_doi = data1, data_score_global = data2, type = "obs", measure = "gini", smooth = TRUE)
#' plot_trend(data_doi = data1, data_score_global = data2, type = "sad", measure = "hhi", smooth = FALSE)
#' plot_trend(data_doi = data1, data_score_global = data2, type = "trd", measure = "entropy", smooth = TRUE)
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#' @importFrom tidyr pivot_longer

plot_trend <- function(data_doi, data_score_global, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_doi)}."))
  if (!is.data.frame(data_score_global)) stop(glue("Error: 'data_score_global' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_score_global)}."))
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
  if (length(measure) > 1) stop(glue("Error: 'measure' must be object of length 1.\nYou provided an object of length {length(measure)}."))
  if (!(measure %in% c("gini", "hhi", "entropy"))) stop(glue("Error: 'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou supplied {measure}."))
  if (length(locations) > 1) stop(glue("Error: 'locations' must be object of length 1.\nYou provided an object of length {length(locations)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be of type 'character'.\nYou supplied an object of type {typeof(locations)}."))
  if (length(smooth) > 1) stop(glue("Error: 'smooth' must be object of length 1.\nYou provided an object of length {length(smooth)}."))
  if (!is.logical(smooth)) stop(glue("Error: 'smooth' must be of type 'logical'.\nYou supplied an object of type {typeof(smooth)}."))

  data_doi <- mutate(data_doi, type = str_replace(type, "score_", ""))
  data_doi$measure <- data_doi[measure][[1]]
  data_score_global$hits <- data_score_global[paste0("score_", type)][[1]]
  data <- full_join(data_doi, data_score_global, by = c("keyword", "date", "object"))
  data <- na.omit(data)

  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data$keyword))

  if (len_keywords > 1) {
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly the first keyword is used."))
    data <- filter(data, keyword %in% unique(data$keyword)[[1]])
  }

  data <- filter(data, type == in_type)
  data <- filter(data, locations == in_locations)

  data <- pivot_longer(data, cols = c(hits, measure), names_to = "plot", values_to = "Trend")
  data$plot[data$plot == "measure"] <- "Degree of internationalization"
  data$plot[data$plot == "hits"] <- "Volume of internationalization"
  plot <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = Trend)) +
    facet_wrap(~plot, scales = "free")

  if (smooth) {
    plot <- plot +
      geom_smooth(aes(y = Trend))
  }

  plot <- plot +
    labs(x = NULL, title = unique(data$keyword)[[1]], caption = glue("DOI computed as {str_to_upper(measure)}."))

  return(plot)
}
