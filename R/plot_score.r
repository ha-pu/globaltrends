#' @title Bar plot of data_score distribution
#'
#' @description
#' @details
#'
#' @param data_score Data exported from \code{export_score} function.
#' @param type Object of class \code{character} indicating the type of time
#' series-column from data_score that is used, takes either "obs", "sad", or
#' "trd". Defaults to "obs".
#'
#' @section Warning:
#' \code{plot_score} is limited to 1 unique keyword to avoid an
#' overcrowded plot. When \code{data_doi} or \code{data_score} includes
#' more than 1 unique keyword, only the first keyword is used.
#'
#' @return Bar plot of score distribution across locations as \code{ggplot2}
#' object.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "manchester united", locations = "countries")
#' plot_score(data_score = data, type = "obs")
#' plot_score(data_score = data, type = "sad")
#' data <- dplyr::filter(data, location %in% countries)
#' plot_score(data_score = data, type = "trd")
#' }
#'
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom stringr str_to_upper

plot_score <- function(data_score, type = "obs") {
  if (!is.data.frame(data_score)) stop(glue("Error: 'data_score' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_score)}."))
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))

  in_type <- type
  len_keywords <- length(unique(data_score$keyword))
  data_score$measure <- data_score[paste0("score_", in_type)][[1]]

  if (len_keywords > 1) {
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly the first keyword is used."))
    data_score <- filter(data_score, keyword %in% unique(data_score$keyword)[[1]])
  }

  keyword <- unique(data_score$keyword)[[1]]
  data_score <- group_by(data_score, location)
  data_score <- summarise(data_score, measure = mean(measure), .groups = "drop")
  data_score <- arrange(data_score, -measure)
  data_score <- slice(data_score, 1:10)
  data_score <- mutate(data_score, location = as_factor(location))

  plot <- ggplot(data_score) +
    geom_col(aes(x = location, y = measure))


  plot <- plot +
    labs(x = NULL, y = "Search trend", title = keyword, caption = glue("Search trend as {str_to_upper(type)} time series."))

  return(plot)
}
