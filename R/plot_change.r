#' @title Plot changes in data
#'
#' @description
#' The function allows to plot changes in search scores, voi, and doi and
#' shows abnormal changes based on their distribution.
#'
#' @details
#'
#' @inheritParams export_control
#' @inheritParams export_doi_change
#' @param data_chnage xxx
#' @param limit xxx
#'
#' @return
#'
#' @seealso
#' * \code{\link{export_doi_change}}
#' * \code{\link{export_voi_change}}
#' * \code{\link[purrr]{map}}
#' * \code{\link[dplyr]{filter}}
#'
#' @examples
#' \dontrun{
#' plot_doi_change(keyword = "amazon", type = "obs", locations = "countries")
#' }
#'
#' @rdname plot_change
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stringr str_to_upper

plot_doi_change <- function(data_change, type = "obs", limit = 0.95) {
  if (!is.data.frame(data_change)) stop(glue("Error: 'data_change' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_change)}."))
  .check_type(type)
  .check_limit(limit)
  
  in_type <- type
  data_change <- filter(data_change, .data$type == glue("score_{in_type}"))

  q1 <- stats::quantile(data_change$doi_change, limit, na.rm = TRUE)
  q2 <- stats::quantile(data_change$doi_change, 1 - limit, na.rm = TRUE)

  ggplot(data_change, aes(x = .data$date, y = .data$doi_change)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data_change, .data$quantile < (1 - limit) | .data$quantile > limit), colour = "firebrick") +
    labs(x = NULL, y = "Change in degree of internationalization")
}

#' @rdname plot_change
#' @export

plot_voi_change <- function(data_change, limit = 0.95) {
  if (!is.data.frame(data_change)) stop(glue("Error: 'data_change' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_change)}."))
  .check_limit(limit)

  q1 <- stats::quantile(data_change$voi_change, limit, na.rm = TRUE)
  q2 <- stats::quantile(data_change$voi_change, 1 - limit, na.rm = TRUE)

  ggplot(data_change, aes(x = .data$date, y = .data$voi_change)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data_change, .data$quantile < (1 - limit) | .data$quantile > limit), colour = "firebrick") +
    labs(x = NULL, y = "Change in volume of internationalization")
}
