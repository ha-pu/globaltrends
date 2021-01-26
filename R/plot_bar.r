#' @title Create barplot for cross-sectional globaltrends data
#'
#' @description
#' The function creates barplots for cross-sectional search score data. It uses
#' the output of `export_score` to prepare a bar plot of search scores for
#' the top 10 countries. For output from `get_abnorm_hist` the plot shows
#' five locations with the highest and lowest abnormal changes each. When the
#' output includes more than one keyword, only the first keyword is used.
#'
#' @inheritParams plot_ts
#'
#' @return
#' Barplot of cross-sectional data as `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_bar(data, type = "obs")
#'
#' data <- export_score(keyword = "amazon")
#' data <- get_abnorm_hist(data, train_win = 12, train_break = 0, type = "obs")
#' plot_bar(data)
#' }
#'
#' @rdname plot_bar
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr slice
#' @importFrom dplyr summarise
#' @importFrom forcats as_factor
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 labs
#' @importFrom glue glue
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stringr str_to_upper
#' @importFrom utils head
#' @importFrom utils tail

plot_bar <- function(data, ...) UseMethod("plot_bar", data)

#' @rdname plot_bar
#' @export

plot_bar.exp_score <- function(data, type = "obs") {
  .check_type(type)

  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }

  data$measure <- data[paste0("score_", type)][[1]]
  data <- group_by(data, .data$location)
  data <- summarise(data, measure = mean(.data$measure), .groups = "drop")

  if (all(is.na(data$measure))) {
    text <- glue("Plot cannot be created.\nThere is no non-missing data for score_{type}.")
    if (type != "obs") {
      text <- glue("{text}\nMaybe time series adjustments were impossible in compute_score due to less than 24 months of data.")
    }
    warning(text)
  } else {
    data <- arrange(data, -.data$measure)
    data <- slice(data, 1:10)
    data <- mutate(data, location = as_factor(.data$location))

    plot <- ggplot(data) +
      geom_col(aes(x = .data$location, y = .data$measure)) +
      labs(
        x = NULL,
        y = "Search score",
        title = keyword,
        caption = glue("Search score as {str_to_upper(type)} time series.")
      )

    return(plot)
  }
}

#' @rdname plot_bar
#' @export

plot_bar.abnorm_score <- function(data) {
  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }

  data <- na.omit(data)
  data <- group_by(data, .data$location)
  data <- summarise(data, score_abnorm = mean(.data$score_abnorm), .groups = "drop")

  data <- arrange(data, -.data$score_abnorm)
  loc_top <- head(data$location, n = 5)
  loc_bottom <- tail(data$location, n = 5)
  data <- filter(data, .data$location %in% c(loc_top, loc_bottom))
  data <- mutate(data, location = as_factor(.data$location))

  plot <- ggplot(data) +
    geom_col(aes(x = .data$location, y = .data$score_abnorm)) +
    labs(
      x = NULL,
      y = "Abnormal changes in search score",
      title = keyword
    )

  return(plot)
}
