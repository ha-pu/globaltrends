#' @title Load doiGT database and tables
#'
#' @description
#' @details
#' @seealso
#'
#' @return
#' The function exports the following objects to .GlobalEnv:
#' \itemize{
#'   \item doiGT_DB A DBIConnection object, as returned by
#'   \code{DBI::dbConnect()}, connecting to the SQLite database in the working
#'   directory
#'   \item data_geo A remote data source pointing to the table "data_geo" in
#'   the connected SQLite database
#'   \item batch_keywords A remote data source pointing to the table "batch_keywords"
#'   in the connected SQLite database
#'   \item batch_time A remote data source pointing to the table "batch_times"
#'   in the connected SQLite database
#'   \item data_agg A remote data source pointing to the table "data_agg" in
#'   the connected SQLite database
#'   \item data_control A remote data source pointing to the table "data_control" in
#'   the connected SQLite database
#'   \item data_mapping A remote data source pointing to the table "data_mapping" in
#'   the connected SQLite database
#'   \item data_object A remote data source pointing to the table "data_object" in
#'   the connected SQLite database
#'   \item data_score A remote data source pointing to the table "data_score" in
#'   the connected SQLite database
#'   \item data_global A remote data source pointing to the table "data_global" in
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
  doiGT_DB <- dbConnect(SQLite(), "db/doiGT_DB.sqlite")
  message("Successfully connected to database.")

  # get tables ----
  data_geo <- tbl(doiGT_DB, "data_geo")
  batch_keywords <- tbl(doiGT_DB, "batch_keywords")
  batch_time <- tbl(doiGT_DB, "batch_time")
  dict_obj <- tbl(doiGT_DB, "dict_obj")

  data_agg <- tbl(doiGT_DB, "data_agg")
  data_control <- tbl(doiGT_DB, "data_control")
  data_mapping <- tbl(doiGT_DB, "data_mapping")
  data_object <- tbl(doiGT_DB, "data_object")
  data_score <- tbl(doiGT_DB, "data_score")
  data_global <- tbl(doiGT_DB, "data_global")

  # load files ----
  lst_wdi <- filter(data_geo, type == "lst_wdi" & share >= 0.001)
  lst_wdi <- collect(lst_wdi)
  lst_wdi <- pull(lst_wdi, geo)
  lst_usa <- filter(data_geo, type == "lst_usa")
  lst_usa <- collect(lst_usa)
  lst_usa <- pull(lst_usa, geo)

  terms_con <- filter(batch_keywords, type == "con")
  terms_con <- collect(terms_con)
  time_con <- filter(batch_time, type == "con")
  time_con <- collect(time_con)
  terms_obj <- filter(batch_keywords, type == "obj")
  terms_obj <- collect(terms_obj)
  time_obj <- filter(batch_time, type == "obj")
  time_obj <- collect(time_obj)
  dict_obj <- collect(dict_obj)

  # write objects to .GlobalEnv ----
  lst_object <- list(doiGT_DB, data_geo, batch_keywords, batch_time, data_agg, data_control, data_mapping, data_object, data_score, data_global, lst_wdi, lst_usa, terms_con, time_con, terms_obj, time_obj, dict_obj)
  names(lst_object) <- list("doiGT_DB", "data_geo", "batch_keywords", "batch_time", "data_agg", "data_control", "data_mapping", "data_object", "data_score", "data_global", "lst_wdi", "lst_usa", "terms_con", "time_con", "terms_obj", "time_obj", "dict_obj")
  invisible(list2env(lst_object, envir = .GlobalEnv))
  message("Successfully exported all objects to .GlobalEnv.")
}
