#' Add keyword synonyms
#'
#' @aliases
#' add_synonym
#' add_synonym.character
#' add_synonym.list
#'
#' @description
#' @details
#'
#' @param keyword Keyword of type \code{character} for which the synonmys are
#' added
#' @param synonym Synonmy of type \code{character}
#'
#' @return
#'
#' @examples
#' \dontrun{
#' add_synonym(keyword = "fc bayern", synonym = "bayern munich")
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom glue glue
#' @importFrom purrr walk
#' @importFrom tibble tibble

add_synonym <- function(keyword, synonym) UseMethod("add_synonym", synonym)

#' @rdname add_synonym
#' @method add_synonym character
#' @export

add_synonym.character <- function(keyword, synonym) {
  if (length(keyword) > 1) stop(glue("Error:'keyword' must be input of length 1.\nYou supplied an input of length {length(keyword)}."))
  if (!is.character(keyword)) stop(glue("Error:'keyword' must of type 'character'.\nYou supplied an input of type {typeof(keyword)}."))
  if (length(synonym) > 1) {
    add_synonym(keyword = keyword, synonym = as.list(synonym))
  } else {
    if (!is.character(synonym)) stop(glue("Error:'synonym' must of type 'character'.\nYou supplied an input of type {typeof(synonym)}."))
    out <- tibble(keyword, synonym)
    dbWriteTable(
      conn = globaltrends_db,
      name = "keyword_synonyms",
      value = out,
      append = TRUE
    )
    keyword_synonyms <- collect(.tbl_synonyms)
    lst_export <- list(keyword_synonyms, keyword_synonyms)
    names(lst_export) <- list("keyword_synonyms", ".keyword_synonyms")
    invisible(list2env(lst_export, envir = .GlobalEnv))
    message(glue("Successfully added synonym | keyword: {keyword} | synonym: {synonym}"))
  }
}

#' @rdname add_synonym
#' @method add_synonym list
#' @export

add_synonym.list <- function(keyword, synonym) {
  walk(synonym, add_synonym, keyword = keyword)
}
