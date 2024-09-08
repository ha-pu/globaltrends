#' @title Compute abnormal changes in data - historic baseline
#'
#' @description
#' The function allows to compute changes in search scores, voi, and doi and
#' shows percentile of changes to identify abnormal changes. In combination with
#' various *write* functions in R, the functions allow exports from the
#' database to local files.
#'
#' @details
#' The function computes abnormal changes in search scores, VOI, or DOI for each
#' date. We define "abnormal" in terms of deviation from a historic baseline
#' value. To compute the historic baseline value, the function computes a moving
#' average. Users can specify the window for moving average training
#' `train_win` and a break between training and the given date
#' `train_break`. Abnormal changes are the difference between the moving
#' average and the respective search score, VOI, or DOI. To highlight abnormal
#' changes, the function computes a historic percentile rank for each abnormal
#' change within the distribution of abnormal changes. Low percentile ranks
#' signify abnormally high negative changes. High percentile ranks signify
#' abnormally high positive changes.
#' The function uses the output from `export_...` functions as input. As
#' `get_abnorm_hist` offers no additional filters, users are advised to use
#' filters in the `export_...` functions or to pre-process data before
#' using `get_abnorm_hist`.
#'
#' @param data Object of class `exp_score`, `exp_voi` or
#' `exp_doi` generated through `export_...` functions.
#'
#' @param train_win Object of type `numeric`. Length of rolling average
#' training window in months. Defaults to 12.
#'
#' @param train_break Object of type `numeric`. Length of break between
#' rolling average training window and date in months. Defaults to 1.
#'
#' @param type Object of type `character` indicating the type of time
#' series-column from data_score, takes either *obs*, *sad*, or
#' *trd*. Defaults to *"obs"*.
#'
#' @param measure Object of type `character` indicating the measure used
#' for DOI computation for which abnormal changes should be analyzed. Takes
#' either *gini*, *hhi*, or *entropy*. Defaults to *"gini"*.
#'
#' @param ...	Further arguments passed to or from other methods.
#'
#' @return
#' The functions export and filter the respective database tables and return
#' objects of class `"tbl_df", "tbl", "data.frame"`.
#' \itemize{
#'   \item Input class `exp_score` computes abnormal changes in search
#'   scores with columns keyword, location, date, control, object, score,
#'   score_abnorm, quantile. Object of class
#'   `c("abnorm_score", "data.frame")`.
#'   \item Input class `exp_voi` computes abnormal changes in VOI with
#'   columns keyword, date, control, object, voi, voi_abnorm, quantile. Object
#'   of class `c("abnorm_voi", "data.frame")`.
#'   \item Input class `exp_doi` computes abnormal changes in DOI with
#'   columns keyword, locations, date, control, object, doi, doi_abnorm,
#'   quantile. Object of class `c("abnorm_doi", "data.frame")`.
#' }
#'
#' @seealso
#' * [export_score()]
#' * [export_voi()]
#' * [export_doi()]
#' * [dplyr::filter()]
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' get_abnorm_hist(data, train_win = 12, train_break = 0, type = "obs")
#'
#' data <- export_voi(keyword = "amazon")
#' get_abnorm_hist(data, train_win = 12, train_break = 0, type = "obs")
#'
#' data <- export_score(keyword = "amazon")
#' get_abnorm_hist(data, train_win = 12, train_break = 0, measure = "gini")
#' }
#'
#' @rdname get_abnorm_hist
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr percent_rank
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
#' @importFrom zoo rollmean

get_abnorm_hist <- function(data, ...) UseMethod("get_abnorm_hist", data)

#' @rdname get_abnorm_hist
#' @export

get_abnorm_hist.exp_score <- function(data, train_win = 12, train_break = 0, ...) {
  .check_length(train_win, 1)
  .check_input(train_win, "numeric")
  .check_length(train_break, 1)
  .check_input(train_break, "numeric")
  data <- group_by(data, .data$keyword, .data$control, .data$location)
  data <- mutate(data, base = rollmean(.data$score, k = train_win, align = "right", fill = NA))
  data <- mutate(data, base = lag(.data$base, n = train_break + 1))
  data <- mutate(data, score_abnorm = .data$score - .data$base)
  data <- mutate(data, quantile = percent_rank(.data$score_abnorm))
  data <- ungroup(data)
  data <- select(
    data,
    keyword,
    location,
    date,
    control,
    object,
    score,
    score_abnorm,
    quantile
  )
  class(data) <- c("abnorm_score", class(data))
  return(data)
}

#' @rdname get_abnorm_hist
#' @export

get_abnorm_hist.exp_voi <- function(data, train_win = 12, train_break = 0, ...) {
  .check_length(train_win, 1)
  .check_input(train_win, "numeric")
  .check_length(train_break, 1)
  .check_input(train_break, "numeric")
  data <- group_by(data, .data$keyword, .data$control)
  data <- mutate(data, base = rollmean(.data$voi, k = train_win, align = "right", fill = NA))
  data <- mutate(data, base = lag(.data$base, n = train_break + 1))
  data <- mutate(data, voi_abnorm = .data$voi - .data$base)
  data <- mutate(data, quantile = percent_rank(.data$voi_abnorm))
  data <- ungroup(data)
  data <- select(
    data,
    keyword,
    date,
    control,
    object,
    voi,
    voi_abnorm,
    quantile
  )
  class(data) <- c("abnorm_voi", class(data))
  return(data)
}

#' @rdname get_abnorm_hist
#' @export

get_abnorm_hist.exp_doi <- function(data, train_win = 12, train_break = 0, measure = c("gini", "hhi", "entropy"), ...) {
  .check_length(train_win, 1)
  .check_input(train_win, "numeric")
  .check_length(train_break, 1)
  .check_input(train_break, "numeric")
  measure <- match.arg(measure)
  data$doi <- data[measure][[1]]
  data <- group_by(data, .data$keyword, .data$control, .data$locations)
  data <- mutate(data, base = rollmean(.data$doi, k = train_win, align = "right", fill = NA))
  data <- mutate(data, base = lag(.data$base, n = train_break + 1))
  data <- mutate(data, doi_abnorm = .data$doi - .data$base)
  data <- mutate(data, quantile = percent_rank(.data$doi_abnorm))
  data <- ungroup(data)
  data <- select(
    data,
    keyword,
    date,
    control,
    object,
    locations,
    doi,
    doi_abnorm,
    quantile
  )
  class(data) <- c("abnorm_doi", class(data))
  return(data)
}
