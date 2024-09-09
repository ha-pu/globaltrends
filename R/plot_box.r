#' @title Create boxplot for time series of globaltrends data
#'
#' @description
#' The function creates boxplots for time series globaltrends data. It uses
#' the output of `export_...` to prepare boxplots for up to nine keywords.
#'
#' @details
#' For output of `export_score`, only data for a single location is shown.
#' When date for more than one location is provided, the function selects only
#' the first location.
#'
#' @inheritParams plot_ts
#'
#' @return
#' Boxplot of time series as `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_box(data)
#'
#' data <- export_voi(keyword = "amazon")
#' plot_box(data)
#'
#' data <- export_doi(keyword = "amazon")
#' plot_box(data, ci = 0.9)
#' }
#'
#' @rdname plot_box
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stringr str_to_upper

plot_box <- function(data, ...) UseMethod("plot_box", data)

#' @rdname plot_box
#' @export

plot_box.exp_score <- function(data, ...) {
  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(paste0("The plot function is limited to 9 keywords.\nYou use ", len_keywords, " keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }

  len_location <- length(unique(data$location))
  location <- unique(data$location)[[1]]
  if (len_location > 1) {
    data <- filter(data, .data$location == !!location)
    warning(paste0("The plot function is limited to 1 location.\nYou use ", len_location, " locations.\nOnly '", location, "' is used."))
  }

  data$measure <- data$score

  if (all(is.na(data$measure))) {
    warning("Plot cannot be created.\nThere is no non-missing data for score.")
  } else {
    plot <- ggplot(data, aes(x = .data$keyword)) +
      geom_boxplot(aes(y = .data$measure)) +
      labs(x = NULL, y = paste0("Search score for ", location, ""))

    return(plot)
  }
}

#' @rdname plot_box
#' @export

plot_box.exp_voi <- function(data, ...) {
  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(paste0("The plot function is limited to 9 keywords.\nYou use ", len_keywords, " keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data$score
  plot <- ggplot(data, aes(x = .data$date))

  if (all(is.na(data$measure))) {
    warning("Plot cannot be created.\nThere is no non-missing data for VOI.")
  } else {
    plot <- ggplot(data, aes(x = .data$keyword)) +
      geom_boxplot(aes(y = .data$measure)) +
      labs(x = NULL, y = "Volume of internationalization")

    return(plot)
  }
}

#' @rdname plot_box
#' @export

plot_box.exp_doi <- function(data, measure = c("gini", "hhi", "entropy"), locations = "countries", ...) {
  measure <- match.arg(measure)
  .check_locations(locations)

  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(paste0("The plot function is limited to 9 keywords.\nYou use ", len_keywords, " keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data[measure][[1]]
  data <- filter(data, .data$locations == !!locations)

  if (all(is.na(data$measure))) {
    warning("Plot cannot be created.\nThere is no non-missing data for DOI.")
  } else {
    plot <- ggplot(data, aes(x = .data$keyword)) +
      geom_boxplot(aes(y = .data$measure)) +
      labs(x = NULL, y = "Degree of internationalization", caption = paste0("DOI computed as ", str_to_upper(measure), "."))

    return(plot)
  }
}
