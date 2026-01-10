#' Example table: keyword batches (`batch_keywords`)
#'
#' @description
#' Example data representing the database table `batch_keywords`.
#' Each row assigns a single `keyword` to a `batch` and a `type`
#' (`"control"` or `"object"`).
#'
#' In a live database, keyword batches are created via [add_keyword()] and are
#' exported to the package environment `gt.env` by [start_db()] as
#' `gt.env$keywords_control` and `gt.env$keywords_object`.
#'
#' @format A tibble with 3 variables:
#' \describe{
#'   \item{type}{Character. Batch type: `"control"` or `"object"`.}
#'   \item{batch}{Integer. Batch identifier within `type`.}
#'   \item{keyword}{Character. Keyword assigned to the batch.}
#' }
#'
#' @seealso
#' [add_keyword()], [start_db()]
#'
#' @name batch_keywords
"example_keywords"

#' Example table: batch time windows (`batch_time`)
#'
#' @description
#' Example data representing the database table `batch_time`.
#' Each row assigns a time window (`start_date`, `end_date`) to a `batch`
#' and a `type` (`"control"` or `"object"`).
#'
#' In a live database, batch time windows are generated when keywords are added
#' (see [add_keyword()]) and are exported to the package environment `gt.env`
#' by [start_db()] as `gt.env$time_control` and `gt.env$time_object`.
#'
#' Dates are stored as `"YYYY-MM"` strings to represent monthly windows.
#'
#' @format A tibble with 4 variables:
#' \describe{
#'   \item{type}{Character. Batch type: `"control"` or `"object"`.}
#'   \item{batch}{Integer. Batch identifier within `type`.}
#'   \item{start_date}{Character. Window start in `"YYYY-MM"`.}
#'   \item{end_date}{Character. Window end in `"YYYY-MM"`.}
#' }
#'
#' @seealso
#' [add_keyword()], [start_db()]
#'
#' @name batch_time
"example_time"

#' Example table: control downloads (`data_control`)
#'
#' @description
#' Example data representing the database table `data_control`.
#' Each row contains Google Trends `hits` for a control `keyword` in a given
#' `location` on a given `date`, along with the control `batch` identifier.
#'
#' In a live database, data are downloaded via [download_control()] and are
#' available through `gt.env$tbl_control` (a lazy table reference) after
#' [start_db()]. Global aggregates use `"world"` as `location`.
#'
#' The example dataset is simulated to resemble real Google Trends output.
#'
#' @format A tibble with 5 variables:
#' \describe{
#'   \item{location}{Character. Location code (e.g., ISO 3166-1 alpha-2 or other
#'   codes supported by Google Trends). Global data uses `"world"`.}
#'   \item{keyword}{Character. Control keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01 (Unix epoch).
#'   Convert with `lubridate::as_date(date)`.}
#'   \item{hits}{Double. Search interest (Google Trends).}
#'   \item{batch}{Integer. Control batch id.}
#' }
#'
#' @source Google Trends. See the Trends UI and documentation.
#'
#' @seealso
#' [download_control()], [start_db()], [dplyr::tbl()]
#'
#' @name data_control
"example_control"

#' Example table: object downloads (`data_object`)
#'
#' @description
#' Example data representing the database table `data_object`.
#' Each row contains Google Trends `hits` for an object `keyword` in a given
#' `location` on a given `date`. Object data (`batch_o`) are downloaded and
#' mapped to a control batch (`batch_c`) for subsequent score computation.
#'
#' In a live database, data are downloaded via [download_object()] and are
#' available through `gt.env$tbl_object` after [start_db()]. Global aggregates
#' use `"world"` as `location`.
#'
#' The example dataset is simulated to resemble real Google Trends output.
#'
#' @format A tibble with 6 variables:
#' \describe{
#'   \item{location}{Character. Location code. Global data uses `"world"`.}
#'   \item{keyword}{Character. Object keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01. Convert with
#'   `lubridate::as_date(date)`.}
#'   \item{hits}{Double. Search interest (Google Trends).}
#'   \item{batch_c}{Integer. Control batch id used for mapping/baseline.}
#'   \item{batch_o}{Integer. Object batch id.}
#' }
#'
#' @source Google Trends. See the Trends UI and documentation.
#'
#' @seealso
#' [download_object()], [start_db()], [dplyr::tbl()]
#'
#' @name data_object
"example_object"

#' Example table: computed scores (`data_score`)
#'
#' @description
#' Example data representing the database table `data_score`.
#' Each row contains a computed `score` for an object `keyword` in a given
#' `location` on a given `date`, along with the associated control batch
#' (`batch_c`) and object batch (`batch_o`).
#'
#' In a live database, scores are computed via [compute_score()] and are
#' available through `gt.env$tbl_score` after [start_db()]. Global aggregates
#' use `"world"` as `location`.
#'
#' The example dataset is simulated to resemble outputs derived from real
#' Google Trends data.
#'
#' @format A tibble with 6 variables:
#' \describe{
#'   \item{location}{Character. Location code. Global data uses `"world"`.}
#'   \item{keyword}{Character. Object keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01. Convert with
#'   `lubridate::as_date(date)`.}
#'   \item{score}{Double. Computed score (mapped/normalized search interest).}
#'   \item{batch_c}{Integer. Control batch id used as baseline.}
#'   \item{batch_o}{Integer. Object batch id.}
#' }
#'
#' @seealso
#' [compute_score()], [compute_voi()], [start_db()], [dplyr::tbl()]
#'
#' @name data_score
"example_score"

#' Example table: degree of internationalization (`data_doi`)
#'
#' @description
#' Example data representing the database table `data_doi`.
#' Each row contains degree-of-internationalization (DOI) metrics for an object
#' `keyword` on a given `date`, computed from the distribution of `data_score`
#' across a specified set of `locations`.
#'
#' DOI is computed via [compute_doi()] and is available through `gt.env$tbl_doi`
#' after [start_db()]. The `batch_c` column indicates the control batch used as
#' baseline, and `batch_o` indicates the object batch.
#'
#' The example dataset is simulated to resemble outputs derived from real
#' Google Trends data.
#'
#' @format A tibble with 8 variables:
#' \describe{
#'   \item{keyword}{Character. Object keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01. Convert with
#'   `lubridate::as_date(date)`.}
#'   \item{gini}{Double. DOI computed from the (inverted) Gini coefficient of the
#'   score distribution.}
#'   \item{hhi}{Double. DOI computed from the (inverted) Herfindahl-Hirschman
#'   index of the score distribution.}
#'   \item{entropy}{Double. DOI computed from the (inverted) entropy of the score
#'   distribution.}
#'   \item{batch_c}{Integer. Control batch id used as baseline.}
#'   \item{batch_o}{Integer. Object batch id.}
#'   \item{locations}{Character. Name of the location set used (e.g.,
#'   `"countries"`, `"us_states"`).}
#' }
#'
#' @seealso
#' [compute_doi()], [start_db()], [dplyr::tbl()]
#'
#' @name data_doi
"example_doi"

#' Default location set: countries
#'
#' @description
#' Character vector of country location codes used by the package as a default
#' location set for cross-country computations.
#'
#' The vector contains ISO 3166-1 alpha-2 country codes selected based on a GDP
#' share threshold (>= 0.1% in 2018) using the World Bank World Development
#' Indicators (WDI). See [countries_wdi] for the underlying WDI country list.
#'
#' To inspect the full set, print `countries` or use `length(countries)`.
#'
#' @format A character vector.
#'
#' @seealso
#' [add_locations()], [start_db()]
#'
#' @name countries
"countries"

#' Country codes and names from WDI
#'
#' @description
#' A data frame of country/location codes and names as provided by the World
#' Bank World Development Indicators (WDI). This object is useful for mapping
#' ISO-style codes to human-readable names when constructing custom location
#' sets.
#'
#' @format A data.frame.
#'
#' @seealso
#' [countries], [add_locations()]
#'
#' @name countries_wdi
"countries_wdi"

#' Default location set: US states
#'
#' @description
#' Character vector of US state-level location codes used by the package.
#'
#' The vector contains ISO 3166-2 codes of the form `"US-XX"` for the 50 US
#' states and `"US-DC"` for the District of Columbia.
#'
#' @format A character vector.
#'
#' @seealso
#' [add_locations()], [start_db()]
#'
#' @name us_states
"us_states"
