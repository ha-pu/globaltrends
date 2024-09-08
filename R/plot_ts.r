#' @title Plot time series of globaltrends data
#'
#' @description
#' The function creates line plots for time series globaltrends data. It uses
#' the output of `export_...` or `get_abnorm_hist` to prepare line plots for up
#' to nine keywords.
#'
#' @details
#' For output of `export_score`, only data for a single location is shown.
#' When date for more than one location is provided, the function selects only
#' the first location.
#' For output of `get_abnorm_hist`, users can specify confidence intervals
#' to highlight abnormal changes in the data.
#'
#' @param data Data exported from `export_...` or `compute_abnorm` functions.
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
#' @param ci Confidence interval within which changes are assumed to be normal.
#' Object of type `double, 0 < ci < 1`. Defaults to *0.95*.
#'
#' @param ...	Further arguments passed to or from other methods.
#'
#' @return
#' Line plot of time series as `ggplot2` object. For plots for output from
#' `get_abnorm_hist` the provided confidence interval is indicated by red dots.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_ts(data)
#'
#' data <- export_voi(keyword = "amazon")
#' data <- get_abnorm_hist(data, train_win = 12, train_break = 0)
#' plot_ts(data)
#'
#' data <- export_doi(keyword = "amazon")
#' data <- get_abnorm_hist(data, train_win = 12, train_break = 0, measure = "gini")
#' plot_ts(data, ci = 0.9)
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
#' @importFrom stats quantile
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

plot_ts.abnorm_score <- function(data, ci = 0.95, ...) {
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1

  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(paste0("The plot function is limited to 1 keyword.\nYou use ", len_keywords, " keywords.\nOnly '", keyword, "' is used."))
  }

  len_location <- length(unique(data$location))
  location <- unique(data$location)[[1]]
  if (len_location > 1) {
    data <- filter(data, .data$location == !!location)
    warning(paste0("The plot function is limited to 1 location.\nYou use ", len_location, " locations.\nOnly '", location, "' is used."))
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
      y = paste0("Abnormal changes in search score for ", location, ""),
      title = keyword
    )
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

plot_ts.abnorm_voi <- function(data, ci = 0.95, ...) {
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1

  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(paste0("The plot function is limited to 1 keyword.\nYou use ", len_keywords, " keywords.\nOnly '", keyword, "' is used."))
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

#' @rdname plot_ts
#' @export

plot_ts.abnorm_doi <- function(data, locations = "countries", ci = 0.95, ...) {
  .check_locations(locations)
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1

  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(paste0("The plot function is limited to 1 keyword.\nYou use ", len_keywords, " keywords.\nOnly '", keyword, "' is used."))
  }

  data <- filter(data, .data$locations == !!locations)

  data <- na.omit(data)
  q1 <- quantile(data$doi_abnorm, ci1)
  q2 <- quantile(data$doi_abnorm, ci2)

  if (all(is.na(data$doi_abnorm))) {
    warning("Plot cannot be created.\nThere is no non-missing data for DOI.")
  } else {
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
}
