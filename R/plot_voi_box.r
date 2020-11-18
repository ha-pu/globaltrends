#' @title Boxplot of VOI time series
#'
#' @description
#' The function uses the output of \code{export_voi} to prepare a box plot of
#' the distribution of volume of internationalization values. When the output
#' includes more than nine keywords, only the first nine keywords are used.
#'
#' @param data_voi Data exported from \code{export_voi} function.
#' @inheritParams plot_doi_box
#'
#' @return Boxplot of VOI distribution as \code{ggplot2} object.
#'
#' @seealso
#' * \code{\link{export_voi}}
#' * \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' \dontrun{
#' data <- export_voi(object = 1)
#' plot_voi_box(
#'   data_voi = data,
#'   type = "obs"
#' )
#' plot_voi_box(
#'   data_voi = data,
#'   type = "sad"
#' )
#' plot_voi_box(
#'   data_voi = data,
#'   type = "trd"
#' )
#' }
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom rlang .data

plot_voi_box <- function(data_voi, type = "obs") {
  if (!is.data.frame(data_voi)) stop(glue("Error: 'data_voi' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_voi)}."))
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))

  in_type <- type
  len_keywords <- length(unique(data_voi$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a boxplot.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_voi <- filter(data_voi, .data$keyword %in% unique(data_voi$keyword)[1:9])
  }
  data_voi$measure <- data_voi[paste0("score_", in_type)][[1]]
  plot <- ggplot(data_voi, aes(x = .data$keyword, y = .data$measure)) +
    geom_boxplot() +
    labs(x = NULL, y = "Volume of internationalization")

  return(plot)
}
