#' batch_keywords
#'
#' The table \emph{batch_keywords} contains the keywords for each batch. Each
#' line contains one \emph{keyword}, the \emph{type} of the batch (i.e. control
#' or object) and the id of the \emph{batch} to which the keyword is assigned.
#' Keywords can be added with the function \code{add_keywords}. The function
#' \code{start_db} exports the table \emph{batch_keywords} as objects
#' \code{keywords_control} and \code{keywords_object} to \code{.GlobalEnv}.
#'
#' @format A tibble with 19 rows and 3 variables:
#' \describe{
#'   \item{type}{Column of type \code{character} showing the type of each batch,
#'   takes "control" for control batches and "object" for object batches.}
#'   \item{batch}{Column of type \code{integer} showing the number of each
#'   batch.}
#'   \item{keyword}{Column of type \code{character} showing the keywords
#'   included in each batch.}
#' }
#'
#' @seealso
#' * \code{\link[globaltrends]{add_keyword}}
"batch_keywords"

#' batch_time
#'
#' The table \emph{batch_time} contains the time period for which data is
#' downloaded for each batch. Each line contains one \emph{time} period, the
#' \emph{type} of the batch (i.e. control or object) and the id of the
#' \emph{batch} to which the time period is assigned. Time frames take the form
#' \code{"YYYY-MM-DD YYYY-MM-DD"}. Time periods are added automatically through
#' the function \code{add_keywords}. The function \code{start_db} exports the
#' table \emph{batch_time} as objects \code{time_control} and \code{time_object}
#' to \code{.GlobalEnv}.
#'
#' @format A tibble with 5 rows and 3 variables:
#' \describe{
#'   \item{type}{Column of type \code{character} showing the type of each batch,
#'   takes "control" for control batches and "object" for object batches.}
#'   \item{batch}{Column of type \code{integer} showing number of each batch.}
#'   \item{time}{Column of type \code{character} showing the time period for
#'   each batch as "YYYY-MM-DD YYYY-MM-DD".}
#' }
#'
#' @seealso
#' * \code{\link[globaltrends]{add_keyword}}
"batch_time"

#' data_control
#'
#' The table \emph{data_control} contains the downloaded data for each control
#' batch. Each line contains the search \emph{hits} for each \emph{keyword} in a
#' control \emph{batch} for a given \emph{location} and \emph{date}. Global data
#' gets the value \emph{world} as location. Data is downloaded and automatically
#' written to the table through the function \code{download_control}. The
#' function \code{start_db} exports the table \emph{data_control} as database
#' connection \code{tbl_control} to \code{.GlobalEnv}. Users can access the
#' database table through \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_control} was simulated based on actual
#' Google Trends data.
#'
#'
#' @format A tibble with 2,400 rows and 5 variables:
#' \describe{
#'   \item{location}{Column of type \code{character} showing the ISO2 code of
#'   the country or region for which the data was downloaded.}
#'   \item{keyword}{Column of type \code{character} showing the keyword for
#'   which the data was downloaded.}
#'   \item{date}{Column of type \code{integer} showing the date for which the
#'   data was downloaded. Can be transformed into date format with
#'   \code{lubridate::as_date}.}
#'   \item{hits}{Column of type \code{double} showing search volumes for the
#'   respective location-keyword-date combination.}
#'   \item{batch}{Column of type \code{integer} showing the number of each
#'   batch.}
#' }
#' @source \url{https://trends.google.com/trends/}
#' @seealso
#' * \code{\link{download_control}}
#' * \code{\link[dplyr]{tbl}}
"data_control"

#' data_object
#'
#' The table \emph{data_object} contains the downloaded data for each object
#' batch. Each line contains the search \emph{hits} for each \emph{keyword} in
#' an object \emph{batch_o} for a given \emph{location} and \emph{date}. The
#' column \emph{batch_c} indicates the control batch to which the data will be
#' mapped. Global data takes the value \emph{world} as location. Data is
#' downloaded and automatically written to the table through the function
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
#'   \item{location}{Column of type \code{character} showing the ISO2 code of
#'   the country or region for which the data was downloaded.}
#'   \item{keyword}{Column of type \code{character} showing the keyword for
#'   which the data was downloaded.}
#'   \item{date}{Column of type \code{integer} showing the date for which the
#'   data was downloaded. Can be transformed into date format with
#'   \code{lubridate::as_date}.}
#'   \item{hits}{Column of type \code{double} showing search volumes for the
#'   respective location-keyword-date combination.}
#'   \item{batch_c}{Column of type \code{integer} showing the number of each
#'   control batch.}
#'   \item{batch_o}{Column of type \code{integer} showing the number of each
#'   object batch.}
#' }
#' @source \url{https://trends.google.com/trends/}
#' @seealso
#' * \code{\link{download_object}}
#' * \code{\link[dplyr]{tbl}}
"data_object"

#' data_score
#'
#' The table \emph{data_score} contains the search scores for each object batch.
#' Each line contains the observed search score (\emph{score_obs}), the
#' seasonally adjusted search score (\emph{score_sad}), and the trend only
#' search score (\emph{score_trd}) for each \emph{keyword} in an object
#' \emph{batch_o} for a given \emph{location} and \emph{date}. The column
#' \emph{batch_c} indicates the control batch that has been used as baseline
#' for mapping. Global data takes the value \emph{world} as location. Search
#' scores are computed and automatically written to the table with the function
#' \code{compute_score}. The function \code{start_db} exports the table
#' \emph{data_score} as database connection \code{tbl_score} to
#' \code{.GlobalEnv}. Users can access the database table through
#' \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_score} was simulated based on actual
#' Google Trends data.
#'
#' @format A tibble with 6,000 rows and 8 variables:
#' \describe{
#'   \item{location}{Column of type \code{character} showing the ISO2 code of
#'   the country or region for which the data was computed.}
#'   \item{keyword}{Column of type \code{character} showing the keyword for
#'   which the data was downloaded.}
#'   \item{date}{Column of type \code{integer} showing the date for which the
#'   data was computed Can be transformed into date format with
#'   \code{lubridate::as_date}.}
#'   \item{score_obs}{Column of type \code{double} showing search score for the
#'   respective location-keyword-date combination - no time series adjustment.}
#'   \item{score_sad}{Column of type \code{double} showing the search score for
#'   the respective location-keyword-date combination - seasonally adjusted time
#'   series.}
#'   \item{score_trd}{Column of type \code{double} showing the search score for
#'   the respective location-keyword-date combination - trend-only time series.}
#'   \item{batch_c}{Column of type \code{integer} showing the number of each
#'   control batch.}
#'   \item{batch_o}{Column of type \code{integer} showing the number of each
#'   object batch.}
#'   \item{synonym}{Column of type \code{integer} showing whether the line will
#'   be aggregated as synonym.}
#' }
#' @seealso
#' * \code{\link{compute_score}}
#' * \code{\link[dplyr]{tbl}}
"data_score"

#' data_doi
#'
#' The table \emph{data_doi} contains the degree of internationalization (DOI)
#' for each object batch. Each line contains the DOI computed as inverted
#' \emph{gini} coefficient, as inverted \emph{hhi}, or inverted \emph{entropy}
#' for each \emph{keyword} in an object \emph{batch_o} for a given \emph{date}
#' and \emph{type} of search score. The column \emph{batch_c} indicates the
#' control batch that has been used as baseline for mapping. Column
#' \emph{locations} indicates which set of locations was used to compute the
#' distribution of search scores. DOI is computed and automatically written to
#' the table with the function \code{compute_doi}. The function \code{start_db}
#' exports the table \emph{data_doi} as database connection \code{tbl_doi} to
#' \code{.GlobalEnv}. Users can access the database table through
#' \code{dplyr::tbl}.
#'
#' The sample data included in \code{data_doi} was simulated based on actual
#' Google Trends data.
#'
#' @format A tibble with 4,320 rows and 9 variables:
#' \describe{
#'   \item{keyword}{Column of type \code{character} showing the keyword for
#'   which the data was computed.}
#'   \item{date}{Column of type \code{integer} showing the date for which the
#'   data was computed Can be transformed into date format with
#'   \code{lubridate::as_date}.}
#'   \item{type}{Column of type \code{character} indicating the type of time
#'   series-column from \code{data_score} that is used for DOI computation,
#'   takes either "socre_obs", "score_sad", or "score_trd".}
#'   \item{gini}{Column of type \code{double} showing the DOI computed as
#'   inverted Gini coefficient of the search score distribution from
#'   \code{data_score}.}
#'   \item{hhi}{Column of type \code{double} showing the DOI computed as
#'   inverted Herfindahl-Hirschman index of the search score distribution from
#'   \code{data_score}.}
#'   \item{entropy}{Column of type \code{double} showing the DOI computed as
#'   inverted Entropy measure for the search score distribution from
#'   \code{data_score}.}
#'   \item{batch_c}{Column of type \code{integer} showing the number of each
#'   control batch.}
#'   \item{batch_o}{Column of type \code{integer} showing the number of each
#'   object batch.}
#'   \item{locations}{Column of type \code{character} showing the list of
#'   locations for which the search score distribution is used.}
#' }
#' @seealso
#' * \code{\link{compute_doi}}
#' * \code{\link[dplyr]{tbl}}
"data_doi"
