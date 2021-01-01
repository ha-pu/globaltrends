#' @title Plot changes in data
#'
#' @aliases
#' plot_abnorm
#' plot_abnorm.abnorm_score
#' plot_abnorm.abnorm_voi
#' plot_abnorm.abnorm_doi
#' 
#' @description
#' The function uses the output of \code{export_voi_change} and
#' \code{export_doi_change} to prepare a time series plot of changes in VOI or
#' DOI. When the output includes more than one keyword, only the first keyword
#' is used. Dots indicate abnormal changes in the underlying time series.
#'
#' @inheritParams export_control
#' @inheritParams compute_abnorm
#' @param data_change  Data exported from \code{compute_abnorm} function.
#' @param ci Confidence interval within which changes are assumed to be normal.
#' Object of class \code{double, 0 < ci < 1}. Defaults to \emph{0.95}.
#'
#' @return
#' Line plot of change time series as \code{ggplot2} object. Changes with
#' percentile ranks outside the provided confidence interval are indicated by
#' red dots.
#'
#' @seealso
#' * \code{\link{compute_abnorm}}
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
#' @rdname plot_abnorm
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
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stringr str_to_upper

plot_abnorm <- function(data, ...) UseMethod("plot_abnorm", data)

#' @rdname plot_abnorm
#' @method plot_abnorm abnorm_score
#' @export

plot_abnorm.abnorm_score <- function(data, ci = 0.95) {
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1
  
  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }
  
  len_location <- length(unique(data$location))
  location <- unique(data$location)[[1]]
  if (len_location > 1) {
    data <- filter(data, .data$location == !!location)
    warning(glue("The plot function is limited to 1 location.\nYou use {len_location} locations.\nOnly '{location}' is used."))
  }
  
  data <- na.omit(data)
  
  q1 <- quantile(data$score_abnorm, ci1)
  q2 <- quantile(data$score_abnorm, ci2)
  
  ggplot(data, aes(x = .data$date, y = .data$score_abnorm)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = glue("Abnormal changes in search score for {location}"),
      title = keyword
    )
}

#' @rdname plot_abnorm
#' @method plot_abnorm abnorm_voi
#' @export

plot_abnorm.abnorm_voi <- function(data, ci = 0.95) {
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1
  
  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }
  
  data <- na.omit(data)
  
  q1 <- quantile(data$voi_abnorm, ci1)
  q2 <- quantile(data$voi_abnorm, ci2)
  
  ggplot(data, aes(x = .data$date, y = .data$voi_abnorm)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = "Abnormal changes in volume of internationalization",
      title = keyword
    )
}

#' @rdname plot_abnorm
#' @method plot_abnorm abnorm_doi
#' @export

plot_abnorm.abnorm_doi <- function(data, type = "obs", ci = 0.95) {
  .check_type(type)
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1
  
  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }
  
  len_locations <- length(unique(data$locations))
  locations <- unique(data$locations)[[1]]
  if (len_locations > 1) {
    data <- filter(data, .data$locations == !!locations)
    warning(glue("The plot function is limited to 1 set of locations\nYou use {len_locations} sets of locations.\nOnly '{locations}' is used."))
  }
  
  score_type <- glue("score_{type}")
  data <- filter(data, .data$type == score_type)
  data <- na.omit(data)
  q1 <- quantile(data$doi_abnorm, ci1)
  q2 <- quantile(data$doi_abnorm, ci2)
  
  ggplot(data, aes(x = .data$date, y = .data$doi_abnorm)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = q1, colour = "blue4", linetype = "dotted") +
    geom_hline(yintercept = q2, colour = "blue4", linetype = "dotted") +
    geom_line() +
    geom_point(data = filter(data, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = "Abnormal changes in degree of internationalization",
      title = keyword
    )
}
