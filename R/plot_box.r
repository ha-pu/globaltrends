#' @title Create boxplot for time series of globaltrends data
#'
#' @description
#' The function creates boxplots for time series globaltrends data. It uses
#' the output of `export_...` or `get_abnorm_hist` to prepare boxplots for up to
#' nine keywords.
#'
#' @details
#' For output of `export_score`, only data for a single location is shown.
#' When date for more than one location is provided, the function selects only
#' the first location.
#' For output of `get_abnorm_hist`, users can specify confidence intervals
#' to highlight abnormal changes in the data.
#' The `plot_xxx_box` functions allow to call `plot_box` on a data object that
#' lost its respective class (e.g., due to a join).
#'
#' @inheritParams plot_ts
#'
#' @return
#' Boxplot of time series as `ggplot2` object. For plots for output from
#' `get_abnorm_hist` the provided confidence interval is indicated by red dots.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_box(data, type = "obs")
#'
#' # for cases where data looses the respective class
#' data <- export_score(keyword = "amazon")
#' plot_score_box(data)
#'
#' data <- export_voi(keyword = "amazon")
#' data <- get_abnorm_hist(data, train_win = 12, train_break = 0, type = "obs")
#' plot_box(data)
#'
#' data <- export_doi(keyword = "amazon")
#' data <- get_abnorm_hist(data, train_win = 12, train_break = 0, measure = "gini")
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
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stringr str_to_upper

plot_box <- function(data, ...) UseMethod("plot_box", data)

#' @rdname plot_box
#' @export

plot_box.exp_score <- function(data, type = "obs") {
  .check_type(type)

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

  if (all(is.na(data$measure))) {
    text <- glue("Plot cannot be created.\nThere is no non-missing data for score_{type}.")
    if (type != "obs") {
      text <- glue("{text}\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data.")
    }
    warning(text)
  } else {
    plot <- ggplot(data, aes(x = .data$keyword)) +
      geom_boxplot(aes(y = .data$measure)) +
      labs(x = NULL, y = glue("Search score for {location}"))

    return(plot)
  }
}

#' @rdname plot_box
#' @export

plot_score_box <- function(data, type = "obs") {
  class(data) <- c("exp_score", class(data))
  plot_box(data, type = type)
}

#' @rdname plot_box
#' @export

plot_box.abnorm_score <- function(data, ci = 0.95) {
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

  ggplot(data, aes(x = .data$keyword, y = .data$score_abnorm)) +
    geom_boxplot() +
    geom_point(data = filter(data, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = glue("Abnormal changes in search score for {location}"),
      title = keyword
    )
}

#' @rdname plot_box
#' @export

plot_abnorm_score_box <- function(data, ci = 0.95) {
  class(data) <- c("abnorm_score", class(data))
  plot_box(data, ci = ci)
}

#' @rdname plot_box
#' @export

plot_box.exp_voi <- function(data, type = "obs") {
  .check_type(type)

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
    plot <- ggplot(data, aes(x = .data$keyword)) +
      geom_boxplot(aes(y = .data$measure)) +
      labs(x = NULL, y = "Volume of internationalization")

    return(plot)
  }
}

#' @rdname plot_box
#' @export

plot_voi_box <- function(data, type = "obs") {
  class(data) <- c("exp_voi", class(data))
  plot_box(data, type = type)
}

#' @rdname plot_box
#' @export

plot_box.abnorm_voi <- function(data, ci = 0.95) {
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

  ggplot(data, aes(x = .data$keyword, y = .data$voi_abnorm)) +
    geom_boxplot() +
    geom_point(data = filter(data, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
    labs(
      x = NULL,
      y = "Abnormal changes in volume of internationalization",
      title = keyword
    )
}

#' @rdname plot_box
#' @export

plot_abnorm_voi_box <- function(data, ci = 0.95) {
  class(data) <- c("abnorm_voi", class(data))
  plot_box(data, ci = ci)
}

#' @rdname plot_box
#' @export

plot_box.exp_doi <- function(data, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  .check_type(type)
  .check_measure(measure)
  .check_locations(locations)

  len_keywords <- length(unique(data$keyword))
  if (len_keywords > 9) {
    warning(glue("The plot function is limited to 9 keywords.\nYou use {len_keywords} keywords.\nOnly the first 9 keywords are used."))
    data <- filter(data, .data$keyword %in% unique(data$keyword)[1:9])
  }
  data$measure <- data[measure][[1]]
  data <- filter(data, .data$type == paste0("score_", !!type))
  data <- filter(data, .data$locations == !!locations)

  if (all(is.na(data$measure))) {
    text <- glue("Plot cannot be created.\nThere is no non-missing data for score_{type}.")
    if (type != "obs") {
      text <- glue("{text}\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data.")
    }
    warning(text)
  } else {
    plot <- ggplot(data, aes(x = .data$keyword)) +
      geom_boxplot(aes(y = .data$measure)) +
      labs(x = NULL, y = "Degree of internationalization", caption = glue("DOI computed as {str_to_upper(measure)}."))

    return(plot)
  }
}

#' @rdname plot_box
#' @export

plot_doi_box <- function(data, type = "obs", measure = "gini", locations = "countries", smooth = TRUE) {
  class(data) <- c("exp_doi", class(data))
  plot_box(data, type = type, measure = measure, locations = locations, smooth = smooth)
}

#' @rdname plot_box
#' @export

plot_box.abnorm_doi <- function(data, type = "obs", locations = "countries", ci = 0.95) {
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
    ggplot(data, aes(x = .data$keyword, y = .data$doi_abnorm)) +
      geom_boxplot() +
      geom_point(data = filter(data, .data$quantile < ci1 | .data$quantile > ci2), colour = "firebrick") +
      labs(
        x = NULL,
        y = "Abnormal changes in degree of internationalization",
        title = keyword
      )
  }
}

#' @rdname plot_box
#' @export

plot_abnorm_doi_box <- function(data, type = "obs", locations = "countries", ci = 0.95) {
  class(data) <- c("abnorm_doi", class(data))
  plot_box(data, type = type, locations = locations, ci = ci)
}
