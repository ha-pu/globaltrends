#' @title Line plots of VOI and DOI time series
#'
#' @description
#' The function uses the outputs of `export_voi` and `export_doi` to prepare a
#' parallel time series plot of volume and degree of internationalization
#' values. When the output includes more than one keyword, only the first
#' keyword is used.
#'
#' @inheritParams plot_ts
#'
#' @param data_voi Data exported from `export_voi` function.
#'
#' @param data_doi Data exported from `export_doi` function.
#'
#' @return Line plot of VOI and DOI time series as `ggplot2` object.
#'
#' @seealso
#' * [export_voi()]
#' * [export_doi()]
#' * [ggplot2::ggplot()]
#'
#' @examples
#' \dontrun{
#' data1 <- export_voi(keyword = "manchester united")
#' data2 <- export_doi(
#'   keyword = "manchester united",
#'   locations = "countries"
#' )
#' plot_voi_doi(
#'   data_voi = data1,
#'   data_doi = data2,
#'   measure = "gini",
#'   smooth = TRUE
#' )
#' plot_voi_doi(
#'   data_voi = data1,
#'   data_doi = data2,
#'   measure = "hhi",
#'   smooth = FALSE
#' )
#' plot_voi_doi(
#'   data_voi = data1,
#'   data_doi = data2,
#'   measure = "entropy",
#'   smooth = TRUE
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
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#' @importFrom tidyr pivot_longer

plot_voi_doi <- function(data_voi, data_doi, measure = c("gini", "hhi", "entropy"), locations = "countries", smooth = TRUE) {
  .check_input(data_voi, "data.frame")
  .check_input(data_doi, "data.frame")
  measure <- match.arg(measure)
  .check_locations(locations)
  stopifnot("`smooth` must be a logical." = is.logical(smooth))

  in_locations <- locations
  data_doi$measure <- data_doi[measure][[1]]
  data_doi <- filter(data_doi, .data$locations == in_locations)
  data <- full_join(data_doi, data_voi, by = c("keyword", "date", "object"), multiple = "error")

  if (all(is.na(data_voi$score)) | all(is.na(data_doi$measure))) {
    text <- "Plot cannot be created."
    if (all(is.na(data_voi$score))) {
      text <- paste0(text, "\nThere is no non-missing data for score in data_voi.")
    }
    if (all(is.na(data_doi$measure))) {
      text <- paste0(text, "\nThere is no non-missing data for score in data_doi.")
    }
    warning(text)
  } else {
    data <- select(
      data,
      keyword,
      date,
      score,
      measure
    )
    data <- stats::na.omit(data)

    len_keywords <- length(unique(data$keyword))

    if (len_keywords > 1) {
      warning(paste0("The plot function is limited to 1 keyword.\nYou use ", len_keywords, " keywords.\nOnly the first keyword is used."))
      data <- filter(data, .data$keyword %in% unique(data$keyword)[[1]])
    }

    data <- pivot_longer(data, cols = c(score, measure), names_to = "plot", values_to = "trend")
    data <- mutate(data, plot = as_factor(.data$plot))
    data <- mutate(data, plot = fct_recode(.data$plot, "Volume of internationalization" = "score", "Degree of internationalization" = "measure"))
    plot <- ggplot(data, aes(x = .data$date)) +
      geom_line(aes(y = .data$trend)) +
      facet_wrap(~ .data$plot, scales = "free")

    if (smooth) {
      plot <- plot +
        geom_smooth(aes(y = .data$trend))
    }

    plot <- plot +
      labs(x = NULL, y = NULL, title = unique(data$keyword)[[1]], caption = paste0("DOI computed as ", str_to_upper(measure), "."))

    return(plot)
  }
}
