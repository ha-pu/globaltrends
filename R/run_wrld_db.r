#' @title Download worlwide score data for object batch
#'
#' @description
#' @details
#'
#' @param object Object batch for which the data is downloaded. Object
#' of class \code{numeric}.
#'
#' @seealso
#'
#' @return
#' Message that data was downloaded successfully. Data is uploaded
#' to data_wrld.
#'
#' @examples
#' \dontrun{
#' data_wrld(object = 1)
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_c

run_wrld <- function(object) {
  terms <- terms_obj$keyword[terms_obj$batch == object]
  terms <- terms[!(terms %in% dict_obj$term2)]
  time <- time_obj$time[time_obj$batch == object]
  if (.test_empty(table = "data_wrld", batch_o = object)) {
    out <- map_dfr(terms, ~ {
      out <- .get_trend(geo = "", term = .x, time = time)
      out <- select(out, -geo)
      message(str_c("Successfully downloaded worldwide data | term: ", which(terms == .x), "/", length(terms), " complete [", object, "|", max(terms_obj$batch), "]"))
      return(out)
    })
    out <- mutate(out, batch = object)
    dbWriteTable(conn = gtrends_db, name = "data_wrld", value = out, append = TRUE)
  }
}
