#' Add keyword synonyms
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
#' @importFrom tibble tibble

add_synonym <- function(keyword, synonym) {
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
