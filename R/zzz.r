gt.env <- new.env(parent = emptyenv())
.onLoad <- function(libname, pkgname) {
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