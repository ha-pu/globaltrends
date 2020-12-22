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
#' @return Line plot of VOI time series as \code{ggplot2} object.
#'
#' @seealso
#' * \code{\link{export_voi}}
#' * \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' \dontrun{
#' data <- export_voi(object = 1)
#' plot_voi_ts(
#'   data_voi = data,
#'   type = "obs",
#'   smooth = TRUE
#' )
#' plot_voi_ts(
#'   data_voi = data,
#'   type = "sad",
#'   smooth = FALSE
#' )
#' plot_voi_ts(
#'   data_voi = data,
#'   type = "trd",
#'   smooth = TRUE
#' )
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
  if (!is.data.frame(data_voi)) stop(glue("Error: 'data_voi' must be object of type 'data.frame'.\nYou provided an object of type {typeof(data_voi)}."))
  .check_type(type)
  .check_smooth(smooth)

  in_type <- type
  len_keywords <- length(unique(data_voi$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a grid.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_voi <- filter(data_voi, .data$keyword %in% unique(data_voi$keyword)[1:9])
  }
  data_voi$measure <- data_voi[paste0("score_", in_type)][[1]]
  plot <- ggplot(data_voi, aes(x = .data$date))

  if (all(is.na(data_voi$measure))) {
    text <- glue("Plot cannot be created.\nThere is no non-missing data for score_{type}.")
    if (type != "obs") {
      text <- glue("{text}\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data.")
    }
    warning(text)
  } else {
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
}
