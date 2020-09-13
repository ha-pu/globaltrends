#' @title Download worlwide score data for object batch
#'
#' @aliases
#' run_wrld
#' run_wrld.numeric
#' run_wrld.list
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
#' data_wrld(object = as.list(1:5))
#' }
#'
#' @export
#' @rdname run_wrld
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_c

run_wrld <- function(object) UseMethod("run_wrld", object)

#' @rdname run_wrld
#' @method run_wrld numeric
#' @export

run_wrld.numeric <- function(object) {
  terms <- terms_obj$keyword[terms_obj$batch == object]
  terms <- terms[!(terms %in% dict_obj$term2)]
  time <- time_obj$time[time_obj$batch == object]
  if (.test_empty(table = "data_wrld", batch_o = object)) {
    out <- map_dfr(terms, ~ {
      out <- .get_trend(geo = "", term = .x, time = time)
      if (!is.null(out)) {
        out <- select(out, keyword, date, hits)
      } else {
        start <- as_date(str_split(time, pattern = " ")[[1]][[1]])
        end <- as_date(str_split(time, pattern = " ")[[1]][[2]])
        out <- tibble(keyword = .x, date = seq.Date(from = start, to = end, by = "month"), hits = 0)
      }
      out <- mutate(out, batch = object)
      message(str_c("Successfully downloaded worldwide data | term: ", which(terms == .x), "/", length(terms), " [", object, "|", max(terms_obj$batch), "]"))
      return(out)
    })
    out <- mutate(out, batch = object)
    dbWriteTable(conn = gtrends_db, name = "data_wrld", value = out, append = TRUE)
  }
}

#' @rdname run_wrld
#' @method run_wrld list
#' @export

run_wrld.list <- function(object) {
  walk(object, run_wrld)
}
