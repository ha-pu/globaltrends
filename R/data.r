#' batch_keywords
#'
#' xxx.
#'
#' @format A tibble with 4 rows and 3 variables:
#' \describe{
#'   \item{type}{\code{character} indicating the type of batch, takes "control"
#'   for control batches and "object" for object batches}
#'   \item{batch}{\code{integer} number of the batch}
#'   \item{keyword}{\code{character} keywords included in the respective batch}
#' }
"batch_keywords"

#' batch_time
#'
#' xxx.
#'
#' @format A tibble with 2 rows and 3 variables:
#' \describe{
#'   \item{type}{\code{character} indicating the type of batch, takes "control"
#'   for control batches and "object" for object batches}
#'   \item{batch}{\code{integer} number of the batch}
#'   \item{time}{\code{character} timeframe for the respective batch as
#'   "YYYY-MM-DD YYYY-MM-DD"}
#' }
"batch_time"

#' data_control
#'
#' xxx.
#'
#' @format A tibble with 2090 rows and 5 variables:
#' \describe{
#'   \item{location}{\code{character} ISO2 code of country or region for which
#'   the data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{hits}{\code{double} search hits for the respective
#'   location-keyword-date combination}
#'   \item{batch}{\code{integer} number of batch}
#' }
#' @source \url{https://trends.google.com/trends/}
"data_control"

#' data_object
#'
#' xxx.
#'
#' @format A tibble with 2090 rows and 5 variables:
#' \describe{
#'   \item{location}{\code{character} ISO2 code of country or region for which
#'   the data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{hits}{\code{double} search hits for the respective
#'   location-keyword-date combination}
#'   \item{batch_c}{\code{integer} number of control batch}
#'   \item{batch_o}{\code{integer} number of object batch}
#' }
#' @source \url{https://trends.google.com/trends/}
"data_object"

#' data_score
#'
#' xxx.
#'
#' @format A tibble with 480 rows and 8 variables:
#' \describe{
#'   \item{location}{\code{character} ISO2 code of country or region for which
#'   the data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{score_obs}{\code{double} search score for the respective
#'   location-keyword-date combination - no time series adjustment}
#'   \item{score_sad}{\code{double} search score for the respective
#'   location-keyword-date combination - seasonally adjusted time series}
#'   \item{score_trd}{\code{double} search score for the respective
#'   location-keyword-date combination - trend-only time series}
#'   \item{batch_c}{\code{integer} number of control batch}
#'   \item{batch_o}{\code{integer} number of object batch}
#'   \item{synonym}{\code{integer} indicator whether line will be aggregated as
#'   synonym}
#' }
"data_score"

#' data_doi
#'
#' xxx.
#'
#' @format A tibble with 288 rows and 8 variables:
#' \describe{
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{type}{\code{character} indicating the type of time series-column from
#'   \code{data_score} that is used for DOI computation, takes either
#'   "socre_obs", "score_sad", or "score_trd"}
#'   \item{gini}{\code{double} DOI computed as inverted Gini coefficient of
#'   search score from \code{data_score}}
#'   \item{hhi}{\code{double} DOI computed as inverted Herfindahl-Hirschman
#'   index of search score from \code{data_score}}
#'   \item{entropy}{\code{double} DOI computed as inverted Entropy measure for
#'   search score from \code{data_score}}
#'   \item{batch_c}{\code{integer} number of control batch}
#'   \item{batch_o}{\code{integer} number of object batch}
#'   \item{locations}{\code{character} list of locations for which the search
#'   score is used}
#' }
"data_doi"
