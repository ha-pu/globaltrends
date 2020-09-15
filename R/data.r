#' batch_keywords
#'
#' xxx.
#'
#' @format A tibble with 4 rows and 3 variables:
#' \describe{
#'   \item{type}{\code{character} indicating the type of batch, takes "con" for
#'   control batches and "obj" for object batches}
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
#'   \item{type}{\code{character} indicating the type of batch, takes "con" for
#'   control batches and "obj" for object batches}
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
#'   \item{geo}{\code{character} ISO2 code of country or region for which the
#'   data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{hits}{\code{integer} search hits for the respective geo-keyword-date
#'   combination}
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
#'   \item{geo}{\code{character} ISO2 code of country or region for which the
#'   data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{hits}{\code{integer} search hits for the respective geo-keyword-date
#'   combination}
#'   \item{batch}{\code{integer} number of batch}
#' }
#' @source \url{https://trends.google.com/trends/}
"data_object"

#' data_mapping
#'
#' xxx.
#'
#' @format A tibble with 2090 rows and 6 variables:
#' \describe{
#'   \item{geo}{\code{character} ISO2 code of country or region for which the
#'   data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{hits}{\code{integer} search hits for the respective geo-keyword-date
#'   combination}
#'   \item{batch_c}{\code{integer} number of control batch}
#'   \item{batch_o}{\code{integer} number of object batch}
#' }
#' @source \url{https://trends.google.com/trends/}
"data_mapping"

#' data_score
#'
#' xxx.
#'
#' @format A tibble with 480 rows and 8 variables:
#' \describe{
#'   \item{geo}{\code{character} ISO2 code of country or region for which the
#'   data was downloaded}
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{score_obs}{\code{double} search search for the respective geo-keyword-date
#'   combination - no time series adjustment}
#'   \item{score_sad}{\code{double} search search for the respective geo-keyword-date
#'   combination - seasonally adjusted time series}
#'   \item{score_trd}{\code{double} search search for the respective geo-keyword-date
#'   combination - trend-only time series}
#'   \item{batch_c}{\code{integer} number of control batch}
#'   \item{batch_o}{\code{integer} number of object batch}
#' }
"data_score"

#' data_agg
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
"data_agg"

#' data_global
#'
#' xxx.
#'
#' @format A tibble with 418 rows and 4 variables:
#' \describe{
#'   \item{keyword}{\code{character} keyword for which the data was downloaded}
#'   \item{date}{date as \code{integer} variable, can be transformed into date
#'   format with \code{lubridate::as_date}}
#'   \item{hits}{\code{integer} worldwide search hits for the respective
#'   keyword-date combination}
#'   \item{batch}{\code{integer} number of batch}
#' }
#' @source \url{https://trends.google.com/trends/}
"data_global"
