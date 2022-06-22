#' @title globaltrends package environment
#'
#' @description
#' The environment `gt.env` contains all package-related data objects, such as
#' the handle for the SQLite database file or connections to tables. The object
#' involve:
#' \itemize{
#'   \item globaltrends_db: Handle for the SQLite database file.
#'   \item tbl_locations: Connection to table that contains the lists of locations saved in the database.
#'   \item tbl_keywords: Connection to table that contains the lists of keywords saved in the database.
#'   \item tbl_time: Connection to table that contains the lists of batch times saved in the database.
#'   \item tbl_synonyms: Connection to table that contains the lists of keyword synonyms saved in the database.
#'   \item tbl_doi: Connection to table that contains the DOI data saved in the database.
#'   \item tbl_control: Connection to table that contains data on search volume for control terms saved in the database.
#'   \item tbl_object: Connection to table that contains data on search volume for object terms saved in the database.
#'   \item tbl_score: Connection to table that contains data on search scores saved in the database.
#'   \item keywords_control: Tibble that contains all keywords per control batch.
#'   \item time_control: Tibble that contains all batch times per control batch.
#'   \item keywords_object: Tibble that contains all keywords per object batch.
#'   \item time_object: Tibble that contains all batch times per object batch.
#'   \item keyword_synonyms: Tibble that contains all keyword/synonym combinations.
#' }
#'
#' @seealso
#' * [example_control()]
#' * [example_object()]
#' * [example_score()]
#' * [example_doi()]
#'
#' @export gt.env

gt.env <- new.env(parent = emptyenv())
.onAttach <- function(libname, pkgname) {
  lst_name <- list(
    "globaltrends_db",
    "tbl_locations",
    "tbl_keywords",
    "tbl_time",
    "tbl_synonyms",
    "tbl_doi",
    "tbl_control",
    "tbl_object",
    "tbl_score",
    "keywords_control",
    "time_control",
    "keywords_object",
    "time_object",
    "keyword_synonyms"
  )
  lst_object <- as.list(rep(TRUE, length(lst_name)))
  names(lst_object) <- lst_name
  invisible(list2env(lst_object, envir = gt.env))
}
