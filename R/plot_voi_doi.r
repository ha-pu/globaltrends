#' @title Line plots of VOI and DOI time series
#'
#' @description
#' The function uses the outputs of \code{export_voi} and \code{export_doi} to
#' prepare a parallel time series plot of volume and degree of
#' internationalization values. When the output includes more than one keyword,
#' only the first keyword is used.
#'
#' @inheritParams plot_doi_box
#' @inheritParams plot_doi_ts
#' @param data_voi Data exported from \code{export_voi} function.
#'
#' @return Line plot of VOI and DOI time series as \code{ggplot2} object.
#'
#' @seealso
#' * \code{\link{export_voi}}
#' * \code{\link{export_doi}}
#' * \code{\link[ggplot2]{ggplot}}
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
#'   type = "obs",
#'   measure = "gini",
#'   smooth = TRUE
#' )
#' plot_voi_doi(
#'   data_voi = data1,
#'   data_doi = data2,
#'   type = "sad",
#'   measure = "hhi",
#'   smooth = FALSE
#' )
#' plot_voi_doi(
#'   data_voi = data1,
#'   data_doi = data2,
#'   type = "trd",
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
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom stringr str_to_upper
#' @importFrom tidyr pivot_longer

plot_voi_doi <- function(data_voi, data_doi, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  if (!is.data.frame(data_doi)) stop(glue("Error: 'data_doi' must be object of type 'data.frame'.\nYou provided an object of type {typeof(data_doi)}."))
  if (!is.data.frame(data_voi)) stop(glue("Error: 'data_voi' must be object of type 'data.frame'.\nYou provided an object of type {typeof(data_voi)}."))
  .check_type(type)
  .check_measure(measure)
  .check_locations(locations)
  .check_smooth(smooth)

  in_type <- type
  in_locations <- locations
  data_doi <- mutate(data_doi, type = str_replace(.data$type, "score_", ""))
  data_doi$measure <- data_doi[measure][[1]]
  data_doi <- filter(data_doi, .data$type == in_type)
  data_doi <- filter(data_doi, .data$locations == in_locations)
  data_voi$hits <- data_voi[paste0("score_", type)][[1]]
  data <- full_join(data_doi, data_voi, by = c("keyword", "date", "object"))

  if (all(is.na(data_voi$hits)) | all(is.na(data_doi$measure))) {
    text <- "Plot cannot be created."
    if (all(is.na(data_voi$hits))) {
      text <- glue("{text}\nThere is no non-missing data for score_{type} in data_voi.")
    }
    if (all(is.na(data_doi$measure))) {
      text <- glue("{text}\nThere is no non-missing data for score_{type} in data_doi.")
    }
    if (type != "obs") {
      text <- glue("{text}\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data.")
    }
    warning(text)
  } else {
    data <- select(
      data,
      .data$keyword,
      .data$date,
      .data$hits,
      .data$measure
    )
    data <- stats::na.omit(data)

    len_keywords <- length(unique(data$keyword))

    if (len_keywords > 1) {
      warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly the first keyword is used."))
      data <- filter(data, .data$keyword %in% unique(data$keyword)[[1]])
    }

    data <- pivot_longer(data, cols = c(.data$hits, .data$measure), names_to = "plot", values_to = "trend")
    data <- mutate(data, plot = as_factor(.data$plot))
    data <- mutate(data, plot = fct_recode(.data$plot, "Volume of internationalization" = "hits", "Degree of internationalization" = "measure"))
    plot <- ggplot(data, aes(x = .data$date)) +
      geom_line(aes(y = .data$trend)) +
      facet_wrap(~ .data$plot, scales = "free")

    if (smooth) {
      plot <- plot +
        geom_smooth(aes(y = .data$trend))
    }

    plot <- plot +
      labs(x = NULL, y = NULL, title = unique(data$keyword)[[1]], caption = glue("DOI computed as {str_to_upper(measure)}."))

    return(plot)
  }
}
