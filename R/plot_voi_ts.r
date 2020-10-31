#' @title Line plot of VOI time series
#'
#' @description
#' The function uses the output of \code{export_voi} to prepare a time series
#' plot of volume of internationalization values. When the output includes more
#' than nine keywords, only the first nine keywords are used.
#'
#' @inheritParams plot_doi_ts
#' @inheritParams plot_voi_box
#'
#' @return Line plot of time series as \code{ggplot2} object.
#'
#' @seealso \code{\link{export_voi}}
#'
#' @examples
#' \dontrun{
#' data <- export_voi(object = 1)
#' plot_voi_ts(data_voi = data, type = "obs", smooth = TRUE)
#' plot_voi_ts(data_voi = data, type = "sad", smooth = FALSE)
#' plot_voi_ts(data_voi = data, type = "trd", smooth = TRUE)
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
#' @importFrom rlang .data

plot_voi_ts <- function(data_voi, type = "obs", smooth = TRUE) {
  if (!is.data.frame(data_voi)) stop(glue("Error: 'data_voi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_voi)}."))
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
  if (length(smooth) > 1) stop(glue("Error: 'smooth' must be object of length 1.\nYou provided an object of length {length(smooth)}."))
  if (!is.logical(smooth)) stop(glue("Error: 'smooth' must be of type 'logical'.\nYou supplied an object of type {typeof(smooth)}."))

  in_type <- type
  len_keywords <- length(unique(data_voi$keyword))
  data_voi$measure <- data_voi[paste0("score_", in_type)][[1]]
  plot <- ggplot(data_voi, aes(x = .data$date))


  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a grid.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_voi <- filter(data_voi, .data$keyword %in% unique(data_voi$keyword)[1:9])
  }
  plot <- plot +
    geom_line(aes(y = .data$measure)) +
    facet_wrap(~ .data$keyword)

  if (smooth) {
    plot <- plot +
      geom_smooth(aes(y = .data$measure))
  }

  plot <- plot +
    labs(x = NULL, y = "Volume of internationalization")

  return(plot)
}
