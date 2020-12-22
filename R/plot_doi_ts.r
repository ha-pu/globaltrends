#' @title Line plot of DOI time series
#'
#' @description
#' The function uses the output of \code{export_doi} to prepare a time series
#' plot of degree of internationalization values. When the output includes more
#' than nine keywords, only the first nine keywords are used.
#'
#' @inheritParams plot_doi_box
#' @param smooth Object of class \code{logical} indicating whether the
#' \code{geom_smooth} function of \code{ggplot2} should be used. Defaults to
#' \code{TRUE}.
#'
#' @return Line plot of DOI time series as \code{ggplot2} object.
#'
#' @seealso
#' * \code{\link{export_doi}}
#' * \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' \dontrun{
#' data <- export_doi(
#'   object = 1,
#'   locations = "countries"
#' )
#' plot_doi_ts(
#'   data_doi = data,
#'   type = "obs",
#'   measure = "gini",
#'   smooth = TRUE
#' )
#' plot_doi_ts(
#'   data_doi = data,
#'   type = "sad",
#'   measure = "hhi",
#'   smooth = FALSE
#' )
#' plot_doi_ts(
#'   data_doi = data,
#'   type = "trd",
#'   measure = "entropy",
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
#' @importFrom stringr str_to_upper

plot_doi_ts <- function(data_doi, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be object of type 'data.frame'.\nYou provided an object of type {typeof(data_doi)}."))
  .check_type(type)
  .check_measure(measure)
  .check_locations(locations)
  .check_smooth(smooth)

  in_type <- type
  in_locations <- locations
  len_keywords <- length(unique(data_doi$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords in a grid.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data_doi <- filter(data_doi, .data$keyword %in% unique(data_doi$keyword)[1:9])
  }
  data_doi$measure <- data_doi[measure][[1]]
  data_doi <- filter(data_doi, .data$type == paste0("score_", in_type))
  data_doi <- filter(data_doi, .data$locations == in_locations)
  plot <- ggplot(data_doi, aes(x = .data$date))

  if (all(is.na(data_doi$measure))) {
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
      labs(x = NULL, y = "Degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))

    return(plot)
  }
}
