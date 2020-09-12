#' @title Aggregate keyword-country data and compute DOI
#'
#' @description
#' @details
#'
#' @param control Control batch for which the search score is used. Object
#' of class \code{numeric}.
#' @param object Object batch for which the keyword-country data
#' is aggregated and DOI is computed.  Object of class \code{numeric}.
#' @param lst_geo List of locations for which the search score is used.
#' Object of class \code{character}.
#'
#' @seealso
#'
#' @return
#' Message that data was aggregated successfully. Data is uploaded
#' to data_agg.
#'
#' @examples
#' \dontrun{
#' data_agg(control = 1, object = 1, lst_geo = "lst_wdi")
#' }
#'
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr bind_rows
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise_if
#' @importFrom dplyr ungroup
#' @importFrom purrr map_dbl
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer

run_agg <- function(control, object, lst_geo = "lst_wdi") {
  if (.test_empty(table = "data_agg", batch_c = control, batch_o = object, lst_geo = lst_geo)) {
    data <- collect(filter(data_score, batch_c == control & batch_o == object))
    data <- filter(data, geo %in% pull(collect(filter(data_geo, type == lst_geo)), geo))

    # run dict replace
    if (any(data$keyword %in% dict_obj$term1)) {
      kw1 <- unique(data$keyword[data$keyword %in% dict_obj$term1])
      out <- map_dfr(kw1, ~ {
        kw2 <- dict_obj$term2[dict_obj$term1 == .x]
        if (!any(kw2 %in% data$keyword)) {
          out <- terms_obj$batch[terms_obj$keyword == kw2]
          out <- filter(data_score, batch_c == control & batch_o == out)
          out <- collect(out)
          out <- out[out$keyword == kw2, ]
          return(out)
        }
      })
      data <- bind_rows(data, out)
      data$keyword <- str_replace_all(data$keyword, set_names(dict_obj$term1[dict_obj$term1 %in% kw1], dict_obj$term2[dict_obj$term1 %in% kw1]))
      data <- group_by(data, geo, date, keyword, batch_c, batch_o)
      data <- summarise_if(data, is.double, sum)
      data <- ungroup(data)
    }
    data <- data[!(data$keyword %in% dict_obj$term2), ]

    # compute doi measures
    out <- pivot_longer(data, cols = contains("score"), names_to = "type", values_to = "score")
    out <- nest(out, data = c(geo, score))
    out <- mutate(out,
      gini = map_dbl(data, ~ .compute_gini(series = .x$score)),
      hhi = map_dbl(data, ~ .compute_hhi(series = .x$score)),
      entropy = map_dbl(data, ~ .compute_entropy(series = .x$score))
    )
    out <- select(out, -data)

    # write data
    out <- mutate(out, batch_c = control, batch_o = object, lst_geo = lst_geo)
    dbWriteTable(conn = gtrends_db, name = "data_agg", value = out, append = TRUE)
  }
  message(str_c("Successfully computed DOI | control: ", control, " | object: ", object, " complete [", object, "|", max(terms_obj$batch), "]"))
}
