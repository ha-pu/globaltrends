#' @internal
#' @noRd
#' @importFrom utils globalVariables

global_vars <- c(
  "batch",
  "batch_c",
  "batch_o",
  "control",
  "country",
  "doi",
  "doi_abnorm",
  "entropy",
  "geo",
  "gini",
  "hhi",
  "hits",
  "iso2c",
  "keyword",
  "location",
  "locations",
  "measure",
  "object",
  "score",
  "score_abnorm",
  "synonym",
  "type",
  "voi",
  "voi_abnorm"
)

globalVariables(global_vars)
