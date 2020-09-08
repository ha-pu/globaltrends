#' @title
#' Load doiGT database and tables
#'
#' @description
#' @details
#' @seealso
#' @return
#'
#' @example gtrends_base()
#'
#' @export
#' @importFrom DBI dbConnect
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr scr_dbi
#' @importFrom dplyr tbl
#' @importFrom RSQLite SQLite

gtrends_base <- function() {
  # connect to db ----
  gtrends_db_file <- "db/gtrends.sqlite"
  gtrends_db <- dbConnect(SQLite(), gtrends_db_file)
  src_dbi(gtrends_db)

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
  lst_object <- list(gtrends_db, data_geo, batch_terms, batch_time, dict_obj, data_agg, data_con, data_map, data_obj, data_score, data_wrld, lst_wdi, lst_usa, terms_con, time_con, terms_obj, time_obj, dict_obj)
  list2env(lst_object, envir = .GlobalEnv)
}
