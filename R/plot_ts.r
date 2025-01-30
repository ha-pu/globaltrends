#' @title Plot time series of globaltrends data
#'
#' @description
#' The function creates line plots for time series globaltrends data. It uses
#' the output of `export_...` to prepare line plots for up to nine keywords.
#'
#' @details
#' For output of `export_score`, only data for a single location is shown.
#' When date for more than one location is provided, the function selects only
#' the first location.
#'
#' @param data Data exported from `export_...` functions.
#'
#' @param measure Object of type `character` indicating the DOI measure,
#' takes either *gini*, *hhi*, or *entropy*. Defaults to *"gini"*.
#'
#' @param locations Object of type `character` indicating for which
#' set of locations should be filtered. Defaults to *"countries"*.
#'
#' @param smooth Object of type `logical` indicating whether the `geom_smooth`
#' function of `ggplot2` should be used. Defaults to `TRUE`.
#'
#' @param ...	Further arguments passed to or from other methods.
#'
#' @return
#' Line plot of time series as `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_ts(data)
#' }
#'
#' @rdname plot_ts
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stringr str_to_upper

plot_ts <- function(data, ...) UseMethod("plot_ts", data)

#' @rdname plot_ts
#' @export

plot_ts.exp_score <- function(data, smooth = TRUE, ...) {
  stopifnot("`smooth` must be a logical." = is.logical(smooth))

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
    plot <- ggplot(data, aes(x = .data$date)) +
      geom_line(aes(y = .data$measure)) +
      facet_wrap(~ .data$keyword)

    if (smooth) {
      plot <- plot +
        geom_smooth(aes(y = .data$measure))
    }

    plot <- plot +
      labs(x = NULL, y = paste0("Search score for ", location, ""))

    return(plot)
  }
}

#' @rdname plot_ts
#' @export

plot_ts.exp_voi <- function(data, smooth = TRUE, ...) {
  stopifnot("`smooth` must be a logical." = is.logical(smooth))

  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(paste0("The plot function is limited to 9 keywords.\nYou use ", len_keywords, " keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data$score

  if (all(is.na(data$measure))) {
    warning("Plot cannot be created.\nThere is no non-missing data for VOI.")
  } else {
    plot <- ggplot(data, aes(x = .data$date)) +
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

#' @rdname plot_ts
#' @export

plot_ts.exp_doi <- function(data, measure = c("gini", "hhi", "entropy"), locations = "countries", smooth = TRUE, ...) {
  measure <- match.arg(measure)
  .check_locations(locations)
  stopifnot("`smooth` must be a logical." = is.logical(smooth))

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
    plot <- ggplot(data, aes(x = .data$date)) +
      geom_line(aes(y = .data$measure)) +
      facet_wrap(~ .data$keyword)

    if (smooth) {
      plot <- plot +
        geom_smooth(aes(y = .data$measure))
    }

    plot <- plot +
      labs(x = NULL, y = "Degree of internationalization", caption = paste0("DOI computed as ", str_to_upper(measure), "."))

    return(plot)
  }
}
