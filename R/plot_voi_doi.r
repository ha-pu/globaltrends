#' @title Line plots of VOI and DOI time series
#'
#' @description
#' The function uses the outputs of \code{export_voi} and \code{export_doi} to
#' prepare a parallel time series plot of volume and degree of
#' internationalization values. When the output includes more than one keyword,
#' only the first keyword is used.
#'
#' @inheritParams plot_doi_box
#' @inheritParams plot_doi_ts
#' @param data_voi Data exported from \code{export_voi} function.
#'
#' @return Line plot of VOI and DOI time series as \code{ggplot2} object.
#'
#' @seealso
#' \code{\link{export_voi}},
#' \code{\link{export_doi}}
#'
#' @examples
#' \dontrun{
#' data1 <- export_voi(keyword = "manchester united")
#' data2 <- export_doi(
#' keyword = "manchester united", 
#' locations = "countries")
#' plot_voi_doi(
#' data_voi = data1,
#' data_doi = data2,
#' type = "obs",
#' measure = "gini",
#' smooth = TRUE
#' )
#' plot_voi_doi(
#' data_voi = data1, 
#' data_doi = data2, 
#' type = "sad", 
#' measure = "hhi", 
#' smooth = FALSE
#' )
#' plot_voi_doi(
#' data_voi = data1, 
#' data_doi = data2,
#'  type = "trd", 
#' measure = "entropy", 
#' smooth = TRUE
#' )
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom forcats as_factor
#' @importFrom forcats fct_recode
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#' @importFrom tidyr pivot_longer

plot_voi_doi <- function(data_voi, data_doi, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_doi)}."))
  if (!is.data.frame(data_voi)) stop(glue("Error: 'data_voi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_voi)}."))
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
  if (length(measure) > 1) stop(glue("Error: 'measure' must be object of length 1.\nYou provided an object of length {length(measure)}."))
  if (!(measure %in% c("gini", "hhi", "entropy"))) stop(glue("Error: 'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou supplied {measure}."))
  if (length(locations) > 1) stop(glue("Error: 'locations' must be object of length 1.\nYou provided an object of length {length(locations)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be of type 'character'.\nYou supplied an object of type {typeof(locations)}."))
  if (length(smooth) > 1) stop(glue("Error: 'smooth' must be object of length 1.\nYou provided an object of length {length(smooth)}."))
  if (!is.logical(smooth)) stop(glue("Error: 'smooth' must be of type 'logical'.\nYou supplied an object of type {typeof(smooth)}."))

  data_doi <- mutate(data_doi, type = str_replace(.data$type, "score_", ""))
  data_doi$measure <- data_doi[measure][[1]]
  data_voi$hits <- data_voi[paste0("score_", type)][[1]]
  data <- full_join(data_doi, data_voi, by = c("keyword", "date", "object"))
  data <- stats::na.omit(data)

  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data$keyword))

  if (len_keywords > 1) {
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly the first keyword is used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[[1]])
  }

  data <- filter(data, .data$type == in_type)
  data <- filter(data, .data$locations == in_locations)

  data <- pivot_longer(data, cols = c(.data$hits, .data$measure), names_to = "plot", values_to = "Trend")
  data <- mutate(data, plot = as_factor(.data$plot))
  data <- mutate(data, plot = fct_recode(.data$plot, "Volume of internationalization" = "hits", "Degree of internationalization" = "measure"))
  plot <- ggplot(data, aes(x = .data$date)) +
    geom_line(aes(y = .data$Trend)) +
    facet_wrap(~ .data$plot, scales = "free")

  if (smooth) {
    plot <- plot +
      geom_smooth(aes(y = .data$Trend))
  }

  plot <- plot +
    labs(x = NULL, y = NULL, title = unique(data$keyword)[[1]], caption = glue("DOI computed as {str_to_upper(measure)}."))

  return(plot)
}
