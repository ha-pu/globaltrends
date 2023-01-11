#' @title Create map for cross-sectional globaltrends data
#'
#' @description
#' The function creates world maps for cross-sectional search score data. It
#' uses the output of `export_score` to prepare a map of search scores. For
#' output from `get_abnorm_hist` the map shows abnormal changes in the
#' respective country search scores. When the output includes more than one
#' keyword, only the first keyword is used.
#'
#' @inheritParams plot_ts
#'
#' @return
#' Map of cross-sectional data as `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' plot_map(data, type = "obs")
#'
#' data <- export_score(keyword = "amazon")
#' data <- get_abnorm_hist(data, train_win = 12, train_break = 0, type = "obs")
#' plot_map(data)
#' }
#'
#' @rdname plot_map
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_map
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 map_data
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme
#' @importFrom glue glue
#' @importFrom maps area.map
#' @importFrom rlang .data
#' @importFrom stats na.omit
#' @importFrom stringr str_to_upper
#' @importFrom tibble as_tibble

plot_map <- function(data, ...) UseMethod("plot_map", data)

#' @rdname plot_map
#' @export

plot_map.exp_score <- function(data, type = c("obs", "sad", "trd"), ...) {
  type <- match.arg(type)

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
    data_wdi <- as_tibble(WDI::WDI_data$country)

    data <- inner_join(data_wdi, data, by = c("iso2c" = "location"))
    data <- select(
      data,
      country,
      iso2c,
      measure
    )

    data_map <- map_data("world")
    data_map <- filter(data_map, .data$region != "Antarctica")

    data <- left_join(data_map, data, by = c("region" = "country"))

    plot <- ggplot(
      data = data,
      aes(
        x = .data$long,
        y = .data$lat,
        group = .data$group,
        map_id = .data$region,
        fill = .data$measure
      )
    ) +
      geom_map(
        map = data,
        colour = "#f2f2f2",
        linewidth = 0.5
      ) +
      scale_x_continuous(breaks = c()) +
      scale_y_continuous(breaks = c()) +
      labs(
        x = NULL,
        y = NULL,
        title = keyword,
        caption = glue("Search score as {str_to_upper(type)} time series."),
        fill = "Search score"
      ) +
      theme(legend.position = "bottom")

    return(plot)
  }
}

#' @rdname plot_map
#' @export

plot_map.abnorm_score <- function(data, ...) {
  len_keywords <- length(unique(data$keyword))
  keyword <- unique(data$keyword)[[1]]
  if (len_keywords > 1) {
    data <- filter(data, .data$keyword == !!keyword)
    warning(glue("The plot function is limited to 1 keyword.\nYou use {len_keywords} keywords.\nOnly '{keyword}' is used."))
  }

  data <- na.omit(data)
  data <- group_by(data, .data$location)
  data <- summarise(data, score_abnorm = mean(.data$score_abnorm), .groups = "drop")

  data_wdi <- as_tibble(WDI::WDI_data$country)

  data <- inner_join(data_wdi, data, by = c("iso2c" = "location"))
  data <- select(
    data,
    country,
    iso2c,
    score_abnorm
  )

  data_map <- map_data("world")
  data_map <- filter(data_map, .data$region != "Antarctica")

  data <- left_join(data_map, data, by = c("region" = "country"))

  plot <- ggplot(
    data = data,
    aes(
      x = .data$long,
      y = .data$lat,
      group = .data$group,
      map_id = .data$region,
      fill = .data$score_abnorm
    )
  ) +
    geom_map(
      map = data,
      colour = "#f2f2f2",
      linewidth = 0.5
    ) +
    scale_x_continuous(breaks = c()) +
    scale_y_continuous(breaks = c()) +
    labs(
      x = NULL,
      y = NULL,
      title = keyword,
      fill = "Abnormal changes in search score"
    ) +
    theme(legend.position = "bottom")

  return(plot)
}
