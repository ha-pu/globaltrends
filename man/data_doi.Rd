% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{data_doi}
\alias{data_doi}
\alias{example_doi}
\title{data_doi}
\format{
A tibble with 4,320 rows and 9 variables:
\describe{
\item{keyword}{Column of type \code{character} showing the keyword for
which the data was computed.}
\item{date}{Column of type \code{integer} showing the date for which the
data was computed Can be transformed into date format with
\code{lubridate::as_date}.}
\item{type}{Column of type \code{character} indicating the type of time
series-column from \code{data_score} that is used for DOI computation,
takes either "score_obs", "score_sad", or "score_trd".}
\item{gini}{Column of type \code{double} showing the DOI computed as
inverted Gini coefficient of the search score distribution from
\code{data_score}.}
\item{hhi}{Column of type \code{double} showing the DOI computed as
inverted Herfindahl-Hirschman index of the search score distribution from
\code{data_score}.}
\item{entropy}{Column of type \code{double} showing the DOI computed as
inverted Entropy measure for the search score distribution from
\code{data_score}.}
\item{batch_c}{Column of type \code{integer} showing the number of each
control batch.}
\item{batch_o}{Column of type \code{integer} showing the number of each
object batch.}
\item{locations}{Column of type \code{character} showing the list of
locations for which the search score distribution is used.}
}
}
\usage{
example_doi
}
\description{
The table \emph{data_doi} contains the degree of internationalization (DOI)
for each object batch. Each line contains the DOI computed as inverted
\emph{gini} coefficient, as inverted \emph{hhi}, or inverted \emph{entropy}
for each \emph{keyword} in an object \emph{batch_o} for a given \emph{date}
and \emph{type} of search score. The column \emph{batch_c} indicates the
control batch that has been used as baseline for mapping. Column
\emph{locations} indicates which set of locations was used to compute the
distribution of search scores. DOI is computed and automatically written to
the table with the function \code{compute_doi}. The function \code{start_db}
exports the table \emph{data_doi} as database connection \code{tbl_doi} to
the package environment \code{gt.env}. Users can access the database table
through \code{dplyr::tbl}.
The sample data included in \code{data_doi} was simulated based on actual
Google Trends data.
}
\seealso{
\itemize{
\item \code{\link[=compute_doi]{compute_doi()}}
\item \code{\link[dplyr:tbl]{dplyr::tbl()}}
}
}
\keyword{datasets}
