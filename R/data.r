#' Example table: keyword batches (`batch_keywords`)
#'
#' @description
#' Example data representing the database table `batch_keywords`.
#' Each row assigns a single `keyword` to a `batch` and a `type`
#' (`"control"` or `"object"`).
#'
#' The example contains one control batch (5 keywords: gmail, maps, translate,
#' wikipedia, youtube) and four object batches (14 object keywords covering
#' football clubs and technology firms), all covering the period 2010-01 to
#' 2019-12.
#'
#' In a live database, keyword batches are created via [add_keyword()] and are
#' exported to the package environment `gt.env` by [start_db()] as
#' `gt.env$keywords_control` and `gt.env$keywords_object`. Control batches hold
#' up to five keywords; object batches hold up to four (one slot is reserved for
#' the overlap keyword used in score mapping).
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
#' and a `type` (`"control"` or `"object"`). Each `(type, batch)` combination
#' has exactly one row.
#'
#' In a live database, batch time windows are generated when keywords are added
#' (see [add_keyword()]) and are exported to the package environment `gt.env`
#' by [start_db()] as `gt.env$time_control` and `gt.env$time_object`.
#'
#' Dates are stored as `"YYYY-MM"` strings to represent monthly windows. To
#' change the time window for an existing batch, all downloads and computations
#' for that batch must be re-run.
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
#' queryable through `gt.env$globaltrends_db` after [start_db()]. Global
#' aggregates use `"world"` as `location`.
#'
#' The example dataset is simulated to resemble real Google Trends output.
#' Simulated values are bounded to the empirical \[min, max\] range observed in
#' actual downloads for each keyword--location pair.
#'
#' @format A tibble with 5 variables:
#' \describe{
#'   \item{location}{Character. Location code (ISO 3166-1 alpha-2 or other
#'   codes supported by Google Trends). Global data uses `"world"`.}
#'   \item{keyword}{Character. Control keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01 (Unix epoch).
#'   Convert with `as.Date(date, origin = "1970-01-01")`.}
#'   \item{hits}{Integer. Relative search interest in \[0, 100\]. Google Trends
#'   normalizes all values within a single query window so the peak observation
#'   equals 100.}
#'   \item{batch}{Integer. Control batch id.}
#' }
#'
#' @source Google Trends (\url{https://trends.google.com}). Simulated to match
#'   empirical distributional statistics from real downloads.
#'
#' @seealso
#' [download_control()], [start_db()]
#'
#' @name data_control
"example_control"

#' Example table: object downloads (`data_object`)
#'
#' @description
#' Example data representing the database table `data_object`.
#' Each row contains Google Trends `hits` for an object `keyword` in a given
#' `location` on a given `date`. Each download pairs an object batch
#' (`batch_o`) with a control batch (`batch_c`): one control keyword is
#' included in every object query so that object and control hits can be
#' mapped onto a common scale during score computation.
#'
#' In a live database, data are downloaded via [download_object()] and are
#' queryable through `gt.env$globaltrends_db` after [start_db()]. Global
#' aggregates use `"world"` as `location`.
#'
#' The example dataset is simulated to resemble real Google Trends output.
#' Simulated values are bounded to the empirical \[min, max\] range observed in
#' actual downloads for each keyword--location pair.
#'
#' @format A tibble with 6 variables:
#' \describe{
#'   \item{location}{Character. Location code. Global data uses `"world"`.}
#'   \item{keyword}{Character. Object keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01. Convert with
#'   `as.Date(date, origin = "1970-01-01")`.}
#'   \item{hits}{Integer. Relative search interest in \[0, 100\] within the
#'   query window. The peak value across all keywords in that query equals 100.}
#'   \item{batch_c}{Integer. Control batch id. Identifies which control batch
#'   was co-downloaded for scale mapping in [compute_score()].}
#'   \item{batch_o}{Integer. Object batch id.}
#' }
#'
#' @source Google Trends (\url{https://trends.google.com}). Simulated to match
#'   empirical distributional statistics from real downloads.
#'
#' @seealso
#' [download_object()], [start_db()]
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
#' Scores are computed by [compute_score()] as:
#' \deqn{score = \frac{hits_o}{\sum_{k \in C} \tilde{hits}_k}}
#' where \eqn{hits_o} are object search volumes and \eqn{\tilde{hits}_k} are
#' control keyword hits mapped to the object scale via an overlap-based
#' benchmark (see Castelnuovo & Tran, 2017). Scores are non-negative; values
#' greater than 1 are possible when object interest exceeds control interest.
#'
#' In a live database, scores are queryable through `gt.env$globaltrends_db`
#' after [start_db()]. Global aggregates use `"world"` as `location`.
#'
#' The example dataset is simulated to resemble outputs derived from real
#' Google Trends data.
#'
#' @format A tibble with 6 variables:
#' \describe{
#'   \item{location}{Character. Location code. Global data uses `"world"`.}
#'   \item{keyword}{Character. Object keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01. Convert with
#'   `as.Date(date, origin = "1970-01-01")`.}
#'   \item{score}{Double. Normalised search interest (object hits divided by
#'   total mapped control hits). Non-negative; 0 when no control data are
#'   available.}
#'   \item{batch_c}{Integer. Control batch id used as baseline.}
#'   \item{batch_o}{Integer. Object batch id.}
#' }
#'
#' @references
#' Castelnuovo, E. & Tran, T. D. (2017). Google It Up! A Google Trends-based
#' Uncertainty index for the United States and Australia. *Economics Letters*,
#' *161*, 149--153. \doi{10.1016/j.econlet.2017.09.032}
#'
#' @seealso
#' [compute_score()], [start_db()]
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
#' DOI captures how evenly search interest is spread across locations: a
#' perfectly uniform score distribution yields the maximum value for each
#' metric; concentration in a single location yields the minimum. Three
#' complementary dispersion measures are provided — see [compute_doi()] for
#' their exact formulae.
#'
#' DOI is computed via [compute_doi()] and is queryable through
#' `gt.env$globaltrends_db` after [start_db()]. The `batch_c` column indicates
#' the control batch used as baseline, and `batch_o` indicates the object batch.
#'
#' The example dataset is simulated to resemble outputs derived from real
#' Google Trends data.
#'
#' @format A tibble with 8 variables:
#' \describe{
#'   \item{keyword}{Character. Object keyword.}
#'   \item{date}{Integer. Date stored as days since 1970-01-01. Convert with
#'   `as.Date(date, origin = "1970-01-01")`.}
#'   \item{gini}{Double. `1 - Gini(score)` across locations. Range \[0, 1\]:
#'   1 = perfectly equal distribution; 0 = all search interest in one location.}
#'   \item{hhi}{Double. `1 - HHI(score)` across locations. Range
#'   \[0, 1 - 1/n\] where n is the number of locations: higher values indicate
#'   more equal distributions.}
#'   \item{entropy}{Double. `H(p) - log(n)` (Shannon entropy deficit).
#'   Range (-Inf, 0\]: 0 = perfectly uniform distribution; more negative values
#'   indicate greater concentration.}
#'   \item{batch_c}{Integer. Control batch id used as baseline.}
#'   \item{batch_o}{Integer. Object batch id.}
#'   \item{locations}{Character. Name of the location set used (e.g.,
#'   `"countries"`, `"us_states"`).}
#' }
#'
#' @references
#' Castelnuovo, E. & Tran, T. D. (2017). Google It Up! A Google Trends-based
#' Uncertainty index for the United States and Australia. *Economics Letters*,
#' *161*, 149--153. \doi{10.1016/j.econlet.2017.09.032}
#'
#' Puhr, H. & Müllner, J. (2022). Let me Google that for you: Capturing
#' internationalization using Google Trends. Available at SSRN:
#' \url{https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3969013}
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
#' The vector contains ISO 3166-1 alpha-2 country codes selected from
#' [countries_wdi] based on a GDP share threshold (>= 0.1% of world GDP in
#' 2018) using World Bank World Development Indicators (WDI). This threshold
#' retains the economically significant countries while keeping query volume
#' manageable. Pass this vector as the `locations` argument to [compute_score()]
#' or [compute_doi()] for standard cross-country analyses.
#'
#' Note that `"NA"` (Namibia's ISO code) is excluded because the Google Trends
#' API cannot handle it; see [add_locations()] for details.
#'
#' @format A character vector of ISO 3166-1 alpha-2 country codes.
#'
#' @examples
#' length(countries)
#' head(countries)
#'
#' @seealso
#' [countries_wdi], [add_locations()], [start_db()]
#'
#' @name countries
"countries"

#' Country codes and names from WDI
#'
#' @description
#' A data frame of country/location codes and names as provided by the World
#' Bank World Development Indicators (WDI). This object is a bundled snapshot
#' of `WDI::WDI_data$country` included to remove the runtime dependency on the
#' `WDI` package. It is useful for mapping ISO-style codes to human-readable
#' country names when inspecting or constructing custom location sets, and for
#' understanding which countries are included in [countries].
#'
#' @format A data frame whose columns follow the conventions of
#'   `WDI::WDI_data$country`. Key columns include `iso2c` (ISO 3166-1 alpha-2
#'   code, matching values in [countries]), `country` (English country name),
#'   and additional World Bank metadata fields.
#'
#' @source World Bank World Development Indicators (WDI),
#'   \url{https://datatopics.worldbank.org/world-development-indicators/}.
#'   Bundled as a static snapshot; for the latest data see the `WDI` R package.
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
#' The vector contains the 51 ISO 3166-2 codes of the form `"US-XX"` for the
#' 50 US states and `"US-DC"` for the District of Columbia. Pass this vector
#' as the `locations` argument to [compute_score()] or [compute_doi()] for
#' within-US analyses.
#'
#' @format A character vector of 51 ISO 3166-2 location codes.
#'
#' @examples
#' length(us_states)
#' head(us_states)
#'
#' @seealso
#' [add_locations()], [start_db()]
#'
#' @name us_states
"us_states"
