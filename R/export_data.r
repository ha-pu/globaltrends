export_doi <- function(keyword = NULL, object = NULL, control = NULL,
                        locations = NULL, from = NULL, to = NULL, type = NULL,
                        file = NULL) {
  if (is.character(keyword)) {
    in_keyword <- keyword
    if (is.null(control)) {
      data_doi %>%
        filter(keyword == in_keyword)
    } else if (.test_batch(control)) {
      in_control <- control
      data_doi %>%
        filter(keyword == in_keyword & control == in_control)
    }
  }
}