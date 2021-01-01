#' @title Plot time series of globaltrends data
#'
#' @aliases
#' plot_ts
#' plot_ts.exp_score
#' plot_ts.abnorm_score
#' plot_ts.exp_voi
#' plot_ts.abnorm_voi
#' plot_ts.exp_doi
#' plot_ts.abnorm_doi
#'
#' @description
#'
#' @param data Data exported from \code{export_...} or \code{compute_abnorm}
#' functions.
#' @param type Object of class \code{character} indicating the type of time
#' series-column from data_score, takes either \emph{obs}, \emph{sad}, or
#' \emph{trd}. Defaults to \emph{"obs"}.
#' @param measure Object of class \code{character} indicating the DOI measure,
#' takes either \emph{gini}, \emph{hhi}, or \emph{entropy}. Defaults to
#' \emph{"gini"}.
#' @param locations Object of class \code{character} indicating for which
#' set of locations should be filtered. Defaults to \emph{"countries"}.
#' @param smooth Object of class \code{logical} indicating whether the
#' \code{geom_smooth} function of \code{ggplot2} should be used. Defaults to
#' \code{TRUE}.
#' @param ci Confidence interval within which changes are assumed to be normal.
#' Object of class \code{double, 0 < ci < 1}. Defaults to \emph{0.95}.
#'
#' @return
#' Line plot of time series as \code{ggplot2} object. For objects of class
#' \code{abnorm_...} changes with percentile ranks outside the provided
#' confidence interval are indicated by red dots.
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
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stringr str_to_upper

plot_ts <- function(data, ...) UseMethod("plot_ts", data)

#' @rdname plot_ts
#' @method plot_ts exp_score
#' @export

plot_ts.exp_score <- function(data, type = "obs", smooth = TRUE) {
  .check_type(type)
  .check_smooth(smooth)

  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }

  len_location <- length(unique(data$location))
  location <- unique(data$location)[[1]]
  if (len_location > 1) {
    data <- filter(data, .data$location == !!location)
    warning(glue("The plot function is limited to 1 location.\nYou use {len_location} locations.\nOnly '{location}' is used."))
  }

  data$measure <- data[paste0("score_", type)][[1]]
  plot <- ggplot(data, aes(x = .data$date))

  if (all(is.na(data$measure))) {
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
      labs(x = NULL, y = glue("Search score for {location}"))

    return(plot)
  }
}

#' @rdname plot_ts
#' @method plot_ts abnorm_score
#' @export

plot_ts.abnorm_score <- function(data, ci = 0.95) {
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

#' @rdname plot_ts
#' @method plot_ts exp_voi
#' @export

plot_ts.exp_voi <- function(data, type = "obs", smooth = TRUE) {
  .check_type(type)
  .check_smooth(smooth)

  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data[paste0("score_", type)][[1]]
  plot <- ggplot(data, aes(x = .data$date))

  if (all(is.na(data$measure))) {
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
      labs(x = NULL, y = "Volume of internationalization")

    return(plot)
  }
}

#' @rdname plot_ts
#' @method plot_ts abnorm_voi
#' @export

plot_ts.abnorm_voi <- function(data, ci = 0.95) {
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

#' @rdname plot_ts
#' @method plot_ts exp_doi
#' @export

plot_ts.exp_doi <- function(data, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  .check_type(type)
  .check_measure(measure)
  .check_locations(locations)
  .check_smooth(smooth)

  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data[measure][[1]]
  data <- filter(data, .data$type == paste0("score_", !!type))
  data <- filter(data, .data$locations == !!locations)
  plot <- ggplot(data, aes(x = .data$date))

  if (all(is.na(data$measure))) {
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

#' @rdname plot_ts
#' @method plot_ts abnorm_doi
#' @export

plot_ts.abnorm_doi <- function(data, type = "obs", locations = "countries", ci = 0.95) {
  .check_type(type)
  .check_locations(locations)
  .check_ci(ci)
  ci1 <- (1 - ci) / 2
  ci2 <- 1 - ci1

  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }

  data <- filter(data, .data$locations == !!locations)

  data <- filter(data, .data$type == paste0("score_", !!type))
  data <- na.omit(data)
  q1 <- quantile(data$doi_abnorm, ci1)
  q2 <- quantile(data$doi_abnorm, ci2)

  if (all(is.na(data$doi_abnorm))) {
    text <- glue("Plot cannot be created.\nThere is no non-missing data for score_{type}.")
    if (type != "obs") {
      text <- glue("{text}\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data.")
    }
    warning(text)
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
