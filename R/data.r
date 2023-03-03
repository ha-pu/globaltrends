#' batch_keywords
#'
#' @description
#' The table *batch_keywords* contains the keywords for each batch. Each
#' line contains one *keyword*, the *type* of the batch (i.e., control
#' or object) and the id of the *batch* to which the keyword is assigned.
#' Keywords can be added with the function `add_keywords`. The function
#' `start_db` exports the table *batch_keywords* as objects
#' `keywords_control` and `keywords_object` to the package environment
#' `gt.env`.
#'
#' Example data for the table *batch_keywords* is available as R object
#' `example_keywords`.
#'
#' @format A tibble with 19 rows and 3 variables:
#' \describe{
#'   \item{type}{Column of type `character` showing the type of each batch,
#'   takes "control" for control batches and "object" for object batches.}
#'   \item{batch}{Column of type `integer` showing the number of each
#'   batch.}
#'   \item{keyword}{Column of type `character` showing the keywords
#'   included in each batch.}
#' }
#'
#' @seealso
#' * [globaltrends::add_control_keyword()]
#'
#' @name batch_keywords
"example_keywords"

#' batch_time
#'
#' @description
#' The table *batch_time* contains the time period for which data is
#' downloaded for each batch. Each line contains one *time* period, the
#' *type* of the batch (i.e., control or object) and the id of the
#' *batch* to which the time period is assigned. Time frames take the form
#' `"YYYY-MM-DD YYYY-MM-DD"`. Time periods are added automatically through
#' the function `add_keywords`. The function `start_db` exports the
#' table *batch_time* as objects `time_control` and `time_object`
#' to `.GlobalEnv`.
#'
#' Example data for the table *batch_time* is available as R object
#' `example_time`.
#'
#' @format A tibble with 5 rows and 3 variables:
#' \describe{
#'   \item{type}{Column of type `character` showing the type of each batch,
#'   takes "control" for control batches and "object" for object batches.}
#'   \item{batch}{Column of type `integer` showing number of each batch.}
#'   \item{time}{Column of type `character` showing the time period for
#'   each batch as "YYYY-MM-DD YYYY-MM-DD".}
#' }
#'
#' @seealso
#' * [globaltrends::add_control_keyword()]
#'
#' @name batch_time
"example_time"

#' data_control
#'
#' @description
#' The table *data_control* contains the downloaded data for each control
#' batch. Each line contains the search *hits* for each *keyword* in a
#' control *batch* for a given *location* and *date*. Global data
#' gets the value *world* as location. Data is downloaded and automatically
#' written to the table through the function `download_control`. The
#' function `start_db` exports the table *data_control* as database
#' connection `tbl_control` to the package environment `gt.env`. Users
#' can access the database table through `dplyr::tbl`.
#' The sample data included in `data_control` was simulated based on actual
#' Google Trends data.
#'
#' Example data for the table *data_control* is available as R object
#' `example_control`.
#'
#' @format A tibble with 2,400 rows and 5 variables:
#' \describe{
#'   \item{location}{Column of type `character` showing the ISO2 code of
#'   the country or region for which the data was downloaded.}
#'   \item{keyword}{Column of type `character` showing the keyword for
#'   which the data was downloaded.}
#'   \item{date}{Column of type `integer` showing the date for which the
#'   data was downloaded. Can be transformed into date format with
#'   `lubridate::as_date`.`
#'   \item{hits}{Column of type `double` showing search volumes for the
#'   respective location-keyword-date combination.}
#'   \item{batch}{Column of type `integer` showing the number of each
#'   batch.}
#' }
#' @source \url{https://trends.google.com/trends/}
#'
#' @seealso
#' * [download_control()]
#' * [dplyr::tbl()]
#'
#' @name data_control
"example_control"

#' data_object
#'
#' @description
#' The table *data_object* contains the downloaded data for each object
#' batch. Each line contains the search *hits* for each *keyword* in
#' an object *batch_o* for a given *location* and *date*. The
#' column *batch_c* indicates the control batch to which the data will be
#' mapped. Global data takes the value *world* as location. Data is
#' downloaded and automatically written to the table through the function
#' `download_object`. The function `start_db` exports the table
#' *data_object* as database connection `tbl_object` to the package
#' environment `gt.env`. Users can access the database table through
#' `dplyr::tbl`.
#' The sample data included in `data_object` was simulated based on actual
#' Google Trends data.
#'
#' Example data for the table *data_object* is available as R object
#' `example_object`.
#'
#' @format A tibble with 8,640 rows and 6 variables:
#' \describe{
#'   \item{location}{Column of type `character` showing the ISO2 code of
#'   the country or region for which the data was downloaded.}
#'   \item{keyword}{Column of type `character` showing the keyword for
#'   which the data was downloaded.}
#'   \item{date}{Column of type `integer` showing the date for which the
#'   data was downloaded. Can be transformed into date format with
#'   `lubridate::as_date`.}
#'   \item{hits}{Column of type `double` showing search volumes for the
#'   respective location-keyword-date combination.}
#'   \item{batch_c}{Column of type `integer` showing the number of each
#'   control batch.}
#'   \item{batch_o}{Column of type `integer` showing the number of each
#'   object batch.}
#' }
#' @source \url{https://trends.google.com/trends/}
#'
#' @seealso
#' * [download_object()]
#' * [dplyr::tbl()]
#'
#' @name data_object
"example_object"

#' data_score
#'
#' @description
#' The table *data_score* contains the search scores for each object batch.
#' Each line contains the observed search score (*score_obs*), the
#' seasonally adjusted search score (*score_sad*), and the trend only
#' search score (*score_trd*) for each *keyword* in an object
#' *batch_o* for a given *location* and *date*. The column
#' *batch_c* indicates the control batch that has been used as baseline
#' for mapping. Global data takes the value *world* as location. Search
#' scores are computed and automatically written to the table with the function
#' `compute_score`. The function `start_db` exports the table
#' *data_score* as database connection `tbl_score` to
#' the package environment `gt.env`. Users can access the database
#' table through `dplyr::tbl`.
#' The sample data included in `data_score` was simulated based on actual
#' Google Trends data.
#'
#' Example data for the table *data_score* is available as R object
#' `example_score`.
#'
#' @format A tibble with 6,000 rows and 8 variables:
#' \describe{
#'   \item{location}{Column of type `character` showing the ISO2 code of
#'   the country or region for which the data was computed.}
#'   \item{keyword}{Column of type `character` showing the keyword for
#'   which the data was downloaded.}
#'   \item{date}{Column of type `integer` showing the date for which the
#'   data was computed Can be transformed into date format with
#'   `lubridate::as_date`.}
#'   \item{score_obs}{Column of type `double` showing search score for the
#'   respective location-keyword-date combination - no time series adjustment.}
#'   \item{score_sad}{Column of type `double` showing the search score for
#'   the respective location-keyword-date combination - seasonally adjusted time
#'   series.}
#'   \item{score_trd}{Column of type `double` showing the search score for
#'   the respective location-keyword-date combination - trend-only time series.}
#'   \item{batch_c}{Column of type `integer` showing the number of each
#'   control batch.}
#'   \item{batch_o}{Column of type `integer` showing the number of each
#'   object batch.}
#'   \item{synonym}{Column of type `integer` showing whether the line will
#'   be aggregated as synonym.}
#' }
#'
#' @seealso
#' * [compute_score()]
#' * [compute_voi()]
#' * [dplyr::tbl()]
#'
#' @name data_score
"example_score"

#' data_doi
#'
#' @description
#' The table *data_doi* contains the degree of internationalization (DOI)
#' for each object batch. Each line contains the DOI computed as inverted
#' *gini* coefficient, as inverted *hhi*, or inverted *entropy*
#' for each *keyword* in an object *batch_o* for a given *date*
#' and *type* of search score. The column *batch_c* indicates the
#' control batch that has been used as baseline for mapping. Column
#' *locations* indicates which set of locations was used to compute the
#' distribution of search scores. DOI is computed and automatically written to
#' the table with the function `compute_doi`. The function `start_db`
#' exports the table *data_doi* as database connection `tbl_doi` to
#' the package environment `gt.env`. Users can access the database table
#' through `dplyr::tbl`.
#' The sample data included in `data_doi` was simulated based on actual
#' Google Trends data.
#'
#' @format A tibble with 4,320 rows and 9 variables:
#' \describe{
#'   \item{keyword}{Column of type `character` showing the keyword for
#'   which the data was computed.}
#'   \item{date}{Column of type `integer` showing the date for which the
#'   data was computed Can be transformed into date format with
#'   `lubridate::as_date`.}
#'   \item{type}{Column of type `character` indicating the type of time
#'   series-column from `data_score` that is used for DOI computation,
#'   takes either "score_obs", "score_sad", or "score_trd".}
#'   \item{gini}{Column of type `double` showing the DOI computed as
#'   inverted Gini coefficient of the search score distribution from
#'   `data_score`.}
#'   \item{hhi}{Column of type `double` showing the DOI computed as
#'   inverted Herfindahl-Hirschman index of the search score distribution from
#'   `data_score`.}
#'   \item{entropy}{Column of type `double` showing the DOI computed as
#'   inverted Entropy measure for the search score distribution from
#'   `data_score`.}
#'   \item{batch_c}{Column of type `integer` showing the number of each
#'   control batch.}
#'   \item{batch_o}{Column of type `integer` showing the number of each
#'   object batch.}
#'   \item{locations}{Column of type `character` showing the list of
#'   locations for which the search score distribution is used.}
#' }
#'
#' @seealso
#' * [compute_doi()]
#' * [dplyr::tbl()]
#'
#' @name data_doi
"example_doi"

#' countries
#'
#' @description
#' A character vector that includes ISO2 codes for all countries with a share in
#' global GDP >= 0.1\% in 2018. Data on GDP is retrieved from the World Bank's
#' World Development Indicators database. The data includes:
#' * AE
#' * AO
#' * AR
#' * AT
#' * AU
#' * BD
#' * BE
#' * BR
#' * CA
#' * CH
#' * CL
#' * CN
#' * CO
#' * CU
#' * CZ
#' * DE
#' * DK
#' * DO
#' * DZ
#' * EC
#' * EG
#' * ES
#' * ET
#' * FI
#' * FR
#' * GB
#' * GR
#' * HK
#' * HU
#' * ID
#' * IE
#' * IL
#' * IN
#' * IQ
#' * IR
#' * IT
#' * JP
#' * KR
#' * KW
#' * KZ
#' * LK
#' * MA
#' * MX
#' * MY
#' * NG
#' * NL
#' * NO
#' * NZ
#' * OM
#' * PE
#' * PH
#' * PK
#' * PL
#' * PR
#' * PT
#' * QA
#' * RO
#' * RU
#' * SA
#' * SD
#' * SE
#' * SG
#' * SK
#' * TH
#' * TR
#' * TW
#' * UA
#' * US
#' * UZ
#' * VN
#' * ZA
#'
#' @name countries
"countries"

#' countries_wdi
#'
#' @description
#' A data.frame that includes ISO2 codes and country names for all countries and
#' locations in the World Bank's World Development Indicators database.
#'
#' @name countries_wdi
"countries_wdi"

#' us_states
#'
#' @description
#' A character vector that includes ISO2 codes for all US federal states and
#' Washington DC. The data includes:
#' * US-AL
#' * US-AK
#' * US-AZ
#' * US-AR
#' * US-CA
#' * US-CO
#' * US-CT
#' * US-DE
#' * US-FL
#' * US-GA
#' * US-HI
#' * US-ID
#' * US-IL
#' * US-IN
#' * US-IA
#' * US-KS
#' * US-KY
#' * US-LA
#' * US-ME
#' * US-MD
#' * US-MA
#' * US-MI
#' * US-MN
#' * US-MS
#' * US-MO
#' * US-MT
#' * US-NE
#' * US-NV
#' * US-NH
#' * US-NJ
#' * US-NM
#' * US-NY
#' * US-NC
#' * US-ND
#' * US-OH
#' * US-OK
#' * US-OR
#' * US-PA
#' * US-RI
#' * US-SC
#' * US-SD
#' * US-TN
#' * US-TX
#' * US-UT
#' * US-VT
#' * US-VA
#' * US-WA
#' * US-WV
#' * US-WI
#' * US-WY
#' * US-DC
#'
#' @name us_states
"us_states"
