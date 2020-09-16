#' @title Download global score data for object batch
#'
#' @aliases
#' download_global
#' download_global.numeric
#' download_global.list
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
#' to data_global.
#'
#' @examples
#' \dontrun{
#' download_global(object = 1)
#' download_global(object = as.list(1:5))
#' }
#'
#' @export
#' @rdname download_global
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom stringr str_split
#' @importFrom tidyr expand_grid
#' @importFrom tidyr pivot_longer

download_global <- function(object) UseMethod("download_global", object)

#' @rdname download_global
#' @method download_global numeric
#' @export

download_global.numeric <- function(object) {
  .test_batch(object)
  terms <- keywords_object$keyword[keywords_object$batch == object]
  terms <- terms[!(terms %in% keyword_synonyms$synonym)]
  time <- time_object$time[time_object$batch == object]
  if (.test_empty(table = "data_global", batch_o = object)) {
    out <- map_dfr(terms, ~ {
      out <- .get_trend(location = "", term = .x, time = time)
      if (!is.null(out)) {
        out <- .reset_date(out)
        out <- nest(out, data = c(date, hits))
        out <- mutate(out, data = map(data, .adjust_ts))
        out <- unnest(out, data)
        out <- mutate(out,
          hits_trd = case_when(
            hits_trd < 0 & hits_sad < 0 ~ 0.1,
            hits_trd < 0 ~ (hits_obs + hits_sad) / 2,
            TRUE ~ hits_trd
          ),
          hits_sad = case_when(
            hits_sad < 0 & hits_trd < 0 ~ 0.1,
            hits_sad < 0 ~ (hits_obs + hits_trd) / 2,
            TRUE ~ hits_sad
          )
        )
        out <- pivot_longer(out, cols = contains("hits"), names_to = "type", values_to = "hits")
        out <- select(out, keyword, date, type, hits)
      } else {
        start <- as_date(str_split(time, pattern = " ")[[1]][[1]])
        end <- as_date(str_split(time, pattern = " ")[[1]][[2]])
        out <- tibble(keyword = .x, date = seq.Date(from = start, to = end, by = "month"), hits = 0)
        out <- expand_grid(out, tibble(type = c("score_obs", "score_sad", "score_trd")))
      }
      out <- mutate(out, batch = object)
      message(glue("Successfully downloaded worldwide data | term: {current}/{total_terms} [{object}/{total_batches}]", current = which(terms == .x), total_terms = length(terms), total_batches = max(keywords_object$batch)))
      return(out)
    })
    out <- mutate(out, batch = object)
    dbWriteTable(conn = doiGT_DB, name = "data_global", value = out, append = TRUE)
  }
}

#' @rdname download_global
#' @method download_global list
#' @export

download_global.list <- function(object) {
  walk(object, download_global)
}
