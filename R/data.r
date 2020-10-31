#' batch_keywords
#'
#' The table \emph{batch_keywords} contains the keywords per batch. Each line
#' contains one \emph{keyword}, the \emph{type} of the batch (i.e. control or
#' object) and the id of the \emph{batch} to which the keyword is assigned.
#' Keywords can be added through \code{add_keywords}. The function
#' \code{start_db} exports the table \emph{batch_keywords} as objects
#' \code{keywords_control} and \code{keywords_object} to \code{.GlobalEnv}.
#'
#' @format A tibble with 19 rows and 3 variables:
#' \describe{
#'   \item{type}{\code{character} indicating the type of batch, takes "control"
#'   for control batches and "object" for object batches}
#'   \item{batch}{\code{integer} number of the batch}
#'   \item{keyword}{\code{character} keywords included in the respective batch}
#' }
#'
#' @seealso \code{\link[globaltrends]{add_keyword}}
"batch_keywords"

#' batch_time
#'
#' The table \emph{batch_time} contains the time frame for which data is
#' downloaded per batch. Each line contains one \emph{time} frame, the
#' \emph{type} of the batch (i.e. control or object) and the id of the
#' \emph{batch} to which the time frame is assigned. Time frames take the form
#' \code{"YYYY-MM-DD YYYY-MM-DD"}. Time frames are added automatically through
#' \code{add_keywords}. The function \code{start_db} exports the table
#' \emph{batch_time} as objects \code{time_control} and \code{time_object} to
#' \code{.GlobalEnv}.
#'
#' @format A tibble with 5 rows and 3 variables:
#' \describe{
#'   \item{type}{\code{character} indicating the type of batch, takes "control"
#'   for control batches and "object" for object batches}
#'   \item{batch}{\code{integer} number of the batch}
#'   \item{time}{\code{character} timeframe for the respective batch as
#'   "YYYY-MM-DD YYYY-MM-DD"}
#' }
#'
#' @seealso \code{\link[globaltrends]{add_keyword}}
"batch_time"

#' data_control
#'
#' The table \emph{data_control} contains the downloaded data for each control
#' batch. Each line contains the search \emph{hits} for each \emph{keyword} in a
#' control \emph{batch} for a given \emph{location} and \emph{date}. Global data
#' gets the value \emph{world} as location. Data is downloaded and automatically
#' written to the table through \code{download_control}. The function
#' \code{start_db} exports the table \emph{data_control} as database connection
#' \code{tbl_control} to \code{.GlobalEnv}. Users can access the database table
#' through \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_control} was simulated based on actual
#' Google Trends data.
#'
#'
#' @format A tibble with 2,400 rows and 5 variables:
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
#' @seealso \code{\link{download_control}}, \code{\link[dplyr]{tbl}}
"data_control"

#' data_object
#'
#' The table \emph{data_object} contains the downloaded data for each object
#' batch. Each line contains the search \emph{hits} for each \emph{keyword} in
#' an object \emph{batch_o} for a given \emph{location} and \emph{date}. The
#' column \emph{batch_c} indicates the control batch to which the data will be
#' mapped. Global data takes the value \emph{world} as location. Data is
#' downloaded and automatically written to the table through
#' \code{download_object}. The function \code{start_db} exports the table
#' \emph{data_object} as database connection \code{tbl_object} to
#' \code{.GlobalEnv}. Users can access the database table through
#' \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_object} was simulated based on actual
#' Google Trends data.
#'
#' @format A tibble with 8,640 rows and 6 variables:
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
#' The table \emph{data_score} contains the search scores for each object batch.
#' Each line contains the search observed search score (\emph{score_obs}), the
#' seasonally adjusted search score (\emph{score_sad}), and the trend only
#' search score (\emph{score_trd}) for each \emph{keyword} in an object
#' \emph{batch_o} for a given \emph{location} and \emph{date}. The column
#' \emph{batch_c} indicates the control batch that has been used as baseline
#' search score. Global data takes the value \emph{world} as location. Search
#' scores are computed automatically written to the table through
#' \code{compute_score}. The function \code{start_db} exports the table
#' \emph{data_score} as database connection \code{tbl_score} to \code{.GlobalEnv}.
#' Users can access the database table through \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_score} was simulated based on actual
#' Google Trends data.
#'
#' @format A tibble with 6,000 rows and 8 variables:
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
#' @seealso \code{\link{compute_score}}, \code{\link[dplyr]{tbl}}
"data_score"

#' data_doi
#'
#' The table \emph{data_doi} contains the degree of internationalization (doi)
#' for each object batch. Each line contains the doi computed as inverted
#' \emph{gini} coefficient, as inverted \emph{hhi}, or inverted \emph{entropy}
#' for each \emph{keyword} in an object \emph{batch_o} for a given \emph{date}
#' and \emph{type} of search score. The column \emph{batch_c} indicates the
#' control batch that has been used as baseline search score. Column
#' \emph{locations} indicates which set of locations was used to compute the
#' distribution of search scores. Degree of internationalization is computed
#' automatically written to the table through \code{compute_doi}. The function
#' \code{start_db} exports the table \emph{data_doi} as database connection
#' \code{tbl_doi} to \code{.GlobalEnv}. Users can access the database table
#' through \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_doi} was simulated based on actual
#' Google Trends data.
#'
#' @format A tibble with 4,320 rows and 9 variables:
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
#' @seealso \code{\link{compute_doi}}, \code{\link[dplyr]{tbl}}
"data_doi"
