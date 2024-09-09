#' @title Create barplot for cross-sectional globaltrends data
#'
#' @description
#' The function creates barplots for cross-sectional search score data. It uses
#' the output of `export_score` to prepare a bar plot of search scores for
#' the top 10 countries. When the output includes more than one keyword,
#' only the first keyword is used.
#'
#' @inheritParams plot_ts
#'
#' @return
#' Barplot of cross-sectional data as `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_bar(data)
#'
#' data <- export_score(keyword = "amazon")
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
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stringr str_to_upper
#' @importFrom utils head
#' @importFrom utils tail

plot_bar <- function(data, ...) UseMethod("plot_bar", data)

#' @rdname plot_bar
#' @export

plot_bar.exp_score <- function(data, ...) {
  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(paste0("The plot function is limited to 1 keyword.\nYou use ", len_keywords, " keywords.\nOnly '", keyword, "' is used."))
  }

  data$measure <- data$score
  data <- group_by(data, .data$location)
  data <- summarise(data, measure = mean(.data$measure), .groups = "drop")

  if (all(is.na(data$measure))) {
    warning("Plot cannot be created.\nThere is no non-missing data for score.")
  } else {
    data <- arrange(data, -.data$measure)
    data <- slice(data, 1:10)
    data <- mutate(data, location = as_factor(.data$location))

    plot <- ggplot(data) +
      geom_col(aes(x = .data$location, y = .data$measure)) +
      labs(
        x = NULL,
        y = "Search score",
        title = keyword
      )

    return(plot)
  }
}
