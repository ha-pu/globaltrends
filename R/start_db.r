#' @title Load globaltrends database and tables
#'
#' @description
#' @details
#' @seealso
#'
#' @return
#' The function exports the following objects to .GlobalEnv:
#' \itemize{
#'   \item globaltrends_db A DBIConnection object, as returned by
#'   \code{DBI::dbConnect()}, connecting to the SQLite database in the working
#'   directory
#'   \item tbl_doi A remote data source pointing to the table "data_doi" in
#'   the connected SQLite database
#'   \item tbl_control A remote data source pointing to the table "data_control" in
#'   the connected SQLite database
#'   \item tbl_mapping A remote data source pointing to the table "data_mapping" in
#'   the connected SQLite database
#'   \item tbl_object A remote data source pointing to the table "data_object" in
#'   the connected SQLite database
#'   \item tbl_score A remote data source pointing to the table "data_score" in
#'   the connected SQLite database
#'   \item tbl_global A remote data source pointing to the table "data_global" in
#'   the connected SQLite database
#'   \item countries A \code{character} vector containing ISO2 country codes of
#'   countries that add at leas 0.1% to global GDP
#'   \item us_states A \code{character} vector containing ISO2 regional codes of
#'   US states
#'   \item keywords_control A \code{tibble} containing keywords of control batches
#'   \item time_control A \code{tibble} containing times of control batches
#'   \item keywords_object A \code{tibble} containing keywords of object batches
#'   \item time_object A \code{tibble} containing times of control batches
#'   \item keyword_synonyms A \code{tibble} containing synonymous keywords
#' }
#'
#' @examples
#' \dontrun{
#' start_db()
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr tbl
#' @importFrom RSQLite SQLite

start_db <- function() {
  # connect to db ----
  globaltrends_db <- dbConnect(SQLite(), "db/globaltrends_db.sqlite")
  message("Successfully connected to database.")

  # get tables ----
  tbl_locations <- tbl(globaltrends_db, "data_locations")
  tbl_keywords <- tbl(globaltrends_db, "batch_keywords")
  tbl_time <- tbl(globaltrends_db, "batch_time")
  keyword_synonyms <- tbl(globaltrends_db, "keyword_synonyms")

  tbl_doi <- tbl(globaltrends_db, "tbl_doi")
  tbl_control <- tbl(globaltrends_db, "tbl_control")
  tbl_mapping <- tbl(globaltrends_db, "tbl_mapping")
  tbl_object <- tbl(globaltrends_db, "tbl_object")
  tbl_score <- tbl(globaltrends_db, "tbl_score")
  tbl_global <- tbl(globaltrends_db, "tbl_global")

  # load files ----
  countries <- filter(tbl_locations, type == "countries")
  countries <- collect(countries)
  countries <- pull(countries, location)
  us_states <- filter(tbl_locations, type == "us_states")
  us_states <- collect(us_states)
  us_states <- pull(us_states, location)

  keywords_control <- filter(tbl_keywords, type == "control")
  keywords_control <- select(keywords_control, -type)
  keywords_control <- collect(keywords_control)
  time_control <- filter(tbl_time, type == "control")
  time_control <- select(time_control, -type)
  time_control <- collect(time_control)
  keywords_object <- filter(tbl_keywords, type == "object")
  keywords_object <- select(keywords_object, -type)
  keywords_object <- collect(keywords_object)
  time_object <- filter(tbl_time, type == "object")
  time_object <- select(time_object, -type)
  time_object <- collect(time_object)
  keyword_synonyms <- collect(keyword_synonyms)

  # write objects to .GlobalEnv ----
  lst_object <- list(
    tbl_locations,
    tbl_keywords,
    tbl_time,
    tbl_doi,
    tbl_control,
    tbl_mapping,
    tbl_object,
    tbl_score,
    tbl_global,
    keywords_control,
    time_control,
    keywords_object,
    time_object,
    keyword_synonyms
  )
  names(lst_object) <- list(
    ".tbl_locations",
    ".tbl_keywords",
    ".tbl_time",
    ".tbl_doi",
    ".tbl_control",
    ".tbl_mapping",
    ".tbl_object",
    ".tbl_score",
    ".tbl_global",
    ".keywords_control",
    ".time_control",
    ".keywords_object",
    ".time_object",
    ".keyword_synonyms"
  )
  invisible(list2env(lst_object, envir = .GlobalEnv))
  lst_object <- list(
    globaltrends_db,
    tbl_doi,
    tbl_control,
    tbl_mapping,
    tbl_object,
    tbl_score,
    tbl_global,
    countries,
    us_states,
    keywords_control,
    time_control,
    keywords_object,
    time_object,
    keyword_synonyms
  )
  names(lst_object) <- list(
    "globaltrends_db",
    "tbl_doi",
    "tbl_control",
    "tbl_mapping",
    "tbl_object",
    "tbl_score",
    "tbl_global",
    "countries",
    "us_states",
    "keywords_control",
    "time_control",
    "keywords_object",
    "time_object",
    "keyword_synonyms"
  )
  invisible(list2env(lst_object, envir = .GlobalEnv))
  message("Successfully exported all objects to .GlobalEnv.")
}
