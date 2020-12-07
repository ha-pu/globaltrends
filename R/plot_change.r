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
#' @param limit xxx
#'
#' @return
#'
#' @seealso
#' * \code{\link{export_doi_change}}
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

plot_doi_change <- function(keyword, control = NULL, locations = NULL, type = "obs", measure = "gini", limit = 0.95) {
  data <- export_doi_change(keyword = keyword, control = control, locations = locations, type = type, measure = measure)

  .check_limit(limit)

  q1 <- stats::quantile(data$doi_change, limit, na.rm = TRUE)
  q2 <- stats::quantile(data$doi_change, 1 - limit, na.rm = TRUE)

  ggplot(data, aes(x = .data$date, y = .data$doi_change)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data, .data$quantile < (1 - limit) | .data$quantile > limit), colour = "firebrick") +
    labs(x = NULL, y = "Change in degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))
}

#' @rdname plot_change
#' @export

plot_voi_change <- function(keyword, control = NULL, type = "obs", limit = 0.95) {
  data <- export_voi_change(keyword = keyword, control = control, type = type)

  .check_limit(limit)

  q1 <- stats::quantile(data$voi_change, limit, na.rm = TRUE)
  q2 <- stats::quantile(data$voi_change, 1 - limit, na.rm = TRUE)

  ggplot(data, aes(x = .data$date, y = .data$voi_change)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data, .data$quantile < (1 - limit) | .data$quantile > limit), colour = "firebrick") +
    labs(x = NULL, y = "Change in volume of internationalization")
}
