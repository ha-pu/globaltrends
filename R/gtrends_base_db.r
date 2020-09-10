#' @title Load doiGT database and tables
#'
#' @description
#' @details
#' @seealso
#'
#' @return
#' The function exports the following objects to .GlobalEnv:
#' \itemize{
#'   \item gtrends_db A DBIConnection object, as returned by
#'   \code{DBI::dbConnect()}, connecting to the SQLite database in the working
#'   directory
#'   \item data_geo A remote data source pointing to the table "data_geo" in
#'   the connected SQLite database
#'   \item batch_terms A remote data source pointing to the table "batch_terms"
#'   in the connected SQLite database
#'   \item batch_time A remote data source pointing to the table "batch_times"
#'   in the connected SQLite database
#'   \item data_agg A remote data source pointing to the table "data_agg" in
#'   the connected SQLite database
#'   \item data_con A remote data source pointing to the table "data_con" in
#'   the connected SQLite database
#'   \item data_map A remote data source pointing to the table "data_map" in
#'   the connected SQLite database
#'   \item data_obj A remote data source pointing to the table "data_obj" in
#'   the connected SQLite database
#'   \item data_score A remote data source pointing to the table "data_score" in
#'   the connected SQLite database
#'   \item data_wrld A remote data source pointing to the table "data_wrld" in
#'   the connected SQLite database
#'   \item lst_wdi A \code{character} vector containing ISO2 country codes of
#'   countries that add at leas 0.1% to global GDP
#'   \item lst_usa A \code{character} vector containing ISO2 regional codes of
#'   US states
#'   \item terms_con A \code{tibble} containing keywords of control batches
#'   \item time_con A \code{tibble} containing times of control batches
#'   \item terms_obj A \code{tibble} containing keywords of object batches
#'   \item time_obj A \code{tibble} containing times of control batches
#'   \item dict_obj A \code{tibble} containing synonymous keywords
#' }
#'
#' @examples
#' \dontrun{
#' gtrends_base()
#' }
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr tbl
#' @importFrom RSQLite SQLite

gtrends_base <- function() {
  # connect to db ----
  gtrends_db_file <- "db/gtrends.sqlite"
  gtrends_db <- dbConnect(SQLite(), gtrends_db_file)

  # get tables ----
  data_geo <- tbl(gtrends_db, "data_geo")
  batch_terms <- tbl(gtrends_db, "batch_terms")
  batch_time <- tbl(gtrends_db, "batch_time")
  dict_obj <- tbl(gtrends_db, "dict_obj")

  data_agg <- tbl(gtrends_db, "data_agg")
  data_con <- tbl(gtrends_db, "data_con")
  data_map <- tbl(gtrends_db, "data_map")
  data_obj <- tbl(gtrends_db, "data_obj")
  data_score <- tbl(gtrends_db, "data_score")
  data_wrld <- tbl(gtrends_db, "data_wrld")

  # load files ----
  lst_wdi <- filter(data_geo, type == "lst_wdi" & share >= 0.001)
  lst_wdi <- collect(lst_wdi)
  lst_wdi <- pull(lst_wdi, geo)
  lst_usa <- filter(data_geo, type == "lst_usa")
  lst_usa <- collect(lst_usa)
  lst_usa <- pull(lst_usa, geo)

  terms_con <- filter(batch_terms, type == "con")
  terms_con <- collect(terms_con)
  time_con <- filter(batch_time, type == "con")
  time_con <- collect(time_con)
  terms_obj <- filter(batch_terms, type == "obj")
  terms_obj <- collect(terms_obj)
  time_obj <- filter(batch_time, type == "obj")
  time_obj <- collect(time_obj)
  dict_obj <- collect(dict_obj)

  # write objects to .GlobalEnv ----
  lst_object <- list(gtrends_db, data_geo, batch_terms, batch_time, data_agg, data_con, data_map, data_obj, data_score, data_wrld, lst_wdi, lst_usa, terms_con, time_con, terms_obj, time_obj, dict_obj)
  names(lst_object) <- list("gtrends_db", "data_geo", "batch_terms", "batch_time", "data_agg", "data_con", "data_map", "data_obj", "data_score", "data_wrld", "lst_wdi", "lst_usa", "terms_con", "time_con", "terms_obj", "time_obj", "dict_obj")
  list2env(lst_object, envir = .GlobalEnv)
}