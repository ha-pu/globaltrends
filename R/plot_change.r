#' @title Plot changes in data
#'
#' @description
#' The function uses the output of \code{export_voi_change} and
#' \code{export_doi_change} to prepare a time series plot of changes in VOI or
#' DOI. When the output includes more than one keyword, only the first keyword
#' is used. Dots indicate abnormal changes in the underlying time series.
#' 
#' @inheritParams export_control
#' @inheritParams export_doi_change
#' @param data_change  Data exported from \code{export_voi_change} or
#' \code{export_doi_change} function.
#' @param ci Confidence interval within which changes are assumed to be normal.
#' Object of class \code{double, 0 < ci < 1}. Defaults to \emph{0.95}.
#'
#' @return
#' Line plot of change time series as \code{ggplot2} object. Changes with
#' percentile ranks outside the provided confidence interval are indicated by
#' red dots.
#'
#' @seealso
#' * \code{\link{export_doi_change}}
#' * \code{\link{export_voi_change}}
#' * \code{\link[purrr]{map}}
#' * \code{\link[dplyr]{filter}}
#'
#' @examples
#' \dontrun{
#' export_voi_change(
#'   keyword = "amazon",
#'   type = "obs"
#' )
#' plot_voi_change(data_change = data)
#' 
#' data <- export_doi_change(
#'   keyword = "amazon",
#'   locations = "countries",
#'   type = "obs",
#'   measure = "gini"
#' )
#' plot_doi_change(data_change = data)
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

plot_voi_change <- function(data_change, ci = 0.95) {
  if (!is.data.frame(data_change)) stop(glue("Error: 'data_change' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_change)}."))
  
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1
  
  len_keywords <- length(unique(data_change$keyword))
  if (len_keywords > 1) {
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly the first keyword is used."))
    data_change <- filter(data_change, .data$keyword %in% unique(data_change$keyword)[[1]])
  }
  keyword <- unique(data_change$keyword)[[1]]
  
  q1 <- stats::quantile(data_change$voi_change, ci1, na.rm = TRUE)
  q2 <- stats::quantile(data_change$voi_change, ci2, na.rm = TRUE)
  
  ggplot(data_change, aes(x = .data$date, y = .data$voi_change)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data_change, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = "Change in volume of internationalization",
      title = keyword
    )
}

#' @rdname plot_change
#' @export

plot_doi_change <- function(data_change, type = "obs", ci = 0.95) {
  if (!is.data.frame(data_change)) stop(glue("Error: 'data_change' must be of type 'data.frame'.\nYou supplied an object of type {typeof(data_change)}."))
  .check_type(type)
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1
  
  len_keywords <- length(unique(data_change$keyword))
  if (len_keywords > 1) {
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly the first keyword is used."))
    data_change <- filter(data_change, .data$keyword %in% unique(data_change$keyword)[[1]])
  }
  keyword <- unique(data_change$keyword)[[1]]
  
  in_type <- type
  data_change <- filter(data_change, .data$type == glue("score_{in_type}"))
  
  q1 <- stats::quantile(data_change$doi_change, ci1, na.rm = TRUE)
  q2 <- stats::quantile(data_change$doi_change, ci2, na.rm = TRUE)
  
  ggplot(data_change, aes(x = .data$date, y = .data$doi_change)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data_change, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = "Change in degree of internationalization",
      title = keyword
    )
}
