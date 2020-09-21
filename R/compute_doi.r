#' @title Aggregate keyword-country data and compute DOI
#'
#' @aliases
#' compute_doi
#' compute_doi.numeric
#' compute_doi.list
#'
#' @description
#' @details
#'
#' @param control Control batch for which the search score is used. Object
#' of class \code{numeric}.
#' @param object Object batch for which the keyword-country data
#' is aggregated and DOI is computed.  Object of class \code{numeric}.
#' @param locations List of locations for which the search score is used.
#' Object of class \code{character}.
#'
#' @seealso
#'
#' @return
#' Message that data was aggregated successfully. Data is uploaded
#' to data_doi.
#'
#' @examples
#' \dontrun{
#' compute_doi(control = 1, object = 1, locations = "countries")
#' compute_doi(control = 1, object = as.list(1:5), locations = "countries")
#' }
#'
#' @export
#' @rdname compute_doi
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr bind_rows
#' @importFrom dplyr collect
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise_if
#' @importFrom dplyr ungroup
#' @importFrom glue glue
#' @importFrom purrr map_dbl
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @importFrom purrr walk
#' @importFrom stringr str_replace_all
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer

compute_doi <- function(control, object, locations = "countries") UseMethod("compute_doi", object)

#' @rdname compute_doi
#' @method compute_doi numeric
#' @export

compute_doi.numeric <- function(control, object, locations = "countries") {
  control <- control[[1]]
  walk(c(control, object), .test_batch)
  if (.test_empty(table = "data_doi", batch_c = control, batch_o = object, locations = locations)) {
    data <- collect(filter(.tbl_score, batch_c == control & batch_o == object))
    data <- filter(
      data,
      location %in% pull(
        collect(
          filter(
            .tbl_locations,
			type == locations
          )
        ),
        location
      )
    )

    # run dict replace
    if (any(data$keyword %in% .keyword_synonyms$keyword)) {
      keyword1 <- unique(data$keyword[data$keyword %in% .keyword_synonyms$keyword])
      out <- map_dfr(keyword1, ~ {
        keyword2 <- .keyword_synonyms$synonym[.keyword_synonyms$keyword == .x]
        if (!any(keyword2 %in% data$keyword)) {
          out <- .keywords_object$batch[.keywords_object$keyword == keyword2]
          out <- filter(data_score, batch_c == control & batch_o == out)
          out <- collect(out)
          out <- out[out$keyword == keyword2, ]
          return(out)
        }
      })
      data <- bind_rows(data, out)
      data$keyword <- str_replace_all(
        data$keyword,
        set_names(
          .keyword_synonyms$keyword[.keyword_synonyms$keyword %in% keyword1],
          .keyword_synonyms$synonym[.keyword_synonyms$keyword %in% keyword1]
        )
      )
      data <- group_by(data, location, date, keyword, batch_c, batch_o)
      data <- summarise_if(data, is.double, sum)
      data <- ungroup(data)
    }
    data <- data[!(data$keyword %in% .keyword_synonyms$synonym), ]

    # compute doi measures
    out <- pivot_longer(data, cols = contains("score"), names_to = "type", values_to = "score")
    out <- nest(out, data = c(location, score))
    out <- mutate(out,
      gini = map_dbl(data, ~ .compute_gini(series = .x$score)),
      hhi = map_dbl(data, ~ .compute_hhi(series = .x$score)),
      entropy = map_dbl(data, ~ .compute_entropy(series = .x$score))
    )
    out <- select(out, date, keyword, type, gini, hhi, entropy)

    # write data
    out <- mutate(out, batch_c = control, batch_o = object, locations = locations)
    dbWriteTable(conn = globaltrends_db, name = "data_doi", value = out, append = TRUE)
  }
  message(glue("Successfully computed DOI | control: {control} | object: {object} [{object}/{total}]", total = max(.keywords_object$batch)))
}

#' @rdname compute_doi
#' @method compute_doi list
#' @export

compute_doi.list <- function(control, object, locations = "countries") {
  walk(object, compute_doi, control = control, locations = locations)
}
