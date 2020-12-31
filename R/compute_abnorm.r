#' @title Compute abnormal changes in data
#'
#' @aliases
#' compute_abnorm
#' compute_abnorm.exp_score
#' compute_abnorm.exp_voi
#' compute_abnorm.exp_doi
#' 
#' @description
#' The function allows to compute changes in search scores, voi, and doi and
#' shows percentile of changes to identify abnormal changes. In combination with
#' various \emph{write} functions in R, the functions allow exports from the
#' database to local files.
#'
#' @details
#' The function computes abnormal changes in search scores, VOI, or DOI for each
#' date. We define "abnormal" in terms of deviation from a baseline value. To
#' compute the baseline value, the function computes a moving average. Users can
#' specify the window for moving average training \code{train_win} and a break
#' between training and the given date \code{train_break}. Abnormal changes are
#' the difference between the moving average and the respective search score,
#' VOI, or DOI. To highlight abnormal changes, the function computes a historic
#' percentile rank for each abnormal change within the distribution of abnormal
#' changes. Low percentile ranks signify abnormally high negative changes. High
#' percentile ranks signify abnormally high positive changes.
#' The function uses the output from \code{export_...} functions as input. As
#' \code{compute_abnorm} offers no additional filters, users are advised to use
#' filters in the \code{export_...} functions or to pre-process data before
#' using \code{compute_abnorm}.
#'
#' @param data Object of class \code{exp_score}, \code{exp_voi} or
#' \code{exp_doi} generated through \code{export_...} functions.
#' @param train_win Object of class \code{numeric}. Length of rolling average
#' training window in months. Defaults to 12.
#' @param train_break Object of class \code{numeric}. Length of break between 
#' rolling average training window and date in months. Defaults to 1.
#' @inheritParams export_control
#' @param measure Object of class \code{character} indicating the measure used
#' for DOI computation for which abnormal changes should be analyzed. Takes
#' either \emph{gini}, \emph{hhi}, or \emph{entropy}. Defaults to \emph{"gini"}.
#'
#' @return
#' The functions export and filter the respective database tables and return
#' objects of class \code{"tbl_df", "tbl", "data.frame"}.
#' \itemize{
#'   \item Input class \code{exp_score} computes abnormal changes in search
#'   scores with columns keyword, location, date, control, object, score,
#'   score_abnorm, quantile. Object of class
#'   \code{c("abnorm_score", "data.frame")}.
#'   \item Input class \code{exp_voi} computes abnormal changes in VOI with
#'   columns keyword, date, control, object, voi, voi_abnorm, quantile. Object
#'   of class \code{c("abnorm_voi", "data.frame")}.
#'   \item Input class \code{exp_doi} computes abnormal changes in DOI with
#'   columns keyword, locations, date, control, object, doi, doi_abnorm,
#'   quantile. Object of class \code{c("abnorm_doi", "data.frame")}.
#' }
#'
#' @seealso
#' * \code{\link{export_score}}
#' * \code{\link{export_voi}}
#' * \code{\link{export_doi}}
#' * \code{\link[dplyr]{filter}}
#'
#' @examples
#' \dontrun{
#' data <- export_score(keyword = "amazon")
#' compute_score(data, type = "obs")
#' 
#' data <- export_voi(keyword = "amazon")
#' compute_voi(data, type = "obs")
#'
#' data <- export_doi(keyword = "amazon", type = "obs")
#' compute_doi(data, measure = "gini")
#' }
#'
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom dplyr percent_rank
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
#' @importFrom zoo rollmean

compute_abnorm <- function(data, train_win = 12, train_break = 0, ...) UseMethod("compute_abnorm", data)

#' @method compute_abnorm exp_score
compute_abnorm.exp_score <- function(data, train_win = 12, train_break = 0, type = "obs") {
  .check_length(train_win, 1)
  .check_input(train_win, "numeric")
  .check_length(train_break, 1)
  .check_input(train_break, "numeric")
  .check_type(type)
  data$score <- data[glue("score_{type}")][[1]]
  data <- group_by(data, .data$keyword, .data$control, .data$location)
  data <- mutate(data, base = rollmean(.data$score, k = train_win, align = "right", fill = NA))
  data <- mutate(data, base = lag(.data$base, n = train_break + 1))
  data <- mutate(data, score_abnorm = .data$score - .data$base)
  data <- mutate(data, quantile = percent_rank(.data$score_abnorm))
  data <- ungroup(data)
  data <- select(
    data,
    .data$keyword,
    .data$location,
    .data$date,
    .data$control,
    .data$object,
    .data$score,
    .data$score_abnorm,
    .data$quantile
  )
  class(data) <- c("abnorm_score", class(data))
  return(data)
}

#' @method compute_abnorm exp_voi
compute_abnorm.exp_voi <- function(data, train_win = 12, train_break = 0, type = "obs") {
  .check_length(train_win, 1)
  .check_input(train_win, "numeric")
  .check_length(train_break, 1)
  .check_input(train_break, "numeric")
  .check_type(type)
  data$voi <- data[glue("score_{type}")][[1]]
  data <- group_by(data, .data$keyword, .data$control)
  data <- mutate(data, base = rollmean(.data$voi, k = train_win, align = "right", fill = NA))
  data <- mutate(data, base = lag(.data$base, n = train_break + 1))
  data <- mutate(data, voi_abnorm = .data$voi - .data$base)
  data <- mutate(data, quantile = percent_rank(.data$voi_abnorm))
  data <- ungroup(data)
  data <- select(
    data,
    .data$keyword,
    .data$date,
    .data$control,
    .data$object,
    .data$voi,
    .data$voi_abnorm,
    .data$quantile
  )
  class(data) <- c("abnorm_voi", class(data))
  return(data)
}

#' @method compute_abnorm exp_doi
compute_abnorm.exp_doi <- function(data, train_win = 12, train_break = 0, measure = "gini") {
  .check_length(train_win, 1)
  .check_input(train_win, "numeric")
  .check_length(train_break, 1)
  .check_input(train_break, "numeric")
  .check_measure(measure)
  data$doi <- data[measure][[1]]
  data <- group_by(data, .data$keyword, .data$type, .data$control, .data$locations)
  data <- mutate(data, base = rollmean(.data$doi, k = train_win, align = "right", fill = NA))
  data <- mutate(data, base = lag(.data$base, n = train_break + 1))
  data <- mutate(data, doi_abnorm = .data$doi - .data$base)
  data <- mutate(data, quantile = percent_rank(.data$doi_abnorm))
  data <- ungroup(data)
  data <- select(
    data,
    .data$keyword,
    .data$date,
    .data$type,
    .data$control,
    .data$object,
    .data$locations,
    .data$doi,
    .data$doi_abnorm,
    .data$quantile
  )
  class(data) <- c("abnorm_doi", class(data))
  return(data)
}
