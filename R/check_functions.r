#' @title Check input
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom glue glue
#' @importFrom rlang as_name
#' @importFrom rlang enquo

.check_input <- function(input, type) {
  name_input <- as_name(enquo(input))
  check <- do.call(glue("is.{type}"), list(input))
  if (!check) {
    stop(glue("Error: '{name_input}' must be object of type {type}.\nYou provided an object of type {typeof(input)}."))
  }
}

#' @title Check length
#'
#' @keywords internal
#' @noRd

.check_length <- function(input, max) {
  name_input <- as_name(enquo(input))
  if (length(input) > max) {
    stop(glue("Error: '{name_input}' must be object of length {max}.\nYou provided an object of length {length(input)}."))
  }
}

#' @title Check confidence interval
#'
#' @keywords internal
#' @noRd
#'

.check_ci <- function(ci) {
  .check_length(input = ci, max = 1)
  .check_input(input = ci, type = "double")
  if (ci <= 0 | ci >= 1) stop(glue("Error: 'ci' must be greater than 0 and less than 1.\nYou provided {ci}."))
}

#' @title Check locations
#'
#' @keywords internal
#' @noRd

.check_locations <- function(locations) {
  .check_length(input = locations, max = 1)
  .check_input(input = locations, type = "character")
}

#' @title check batch
#'
#' @keywords internal
#' @noRd

.check_batch <- function(batch) {
  if (is.null(batch)) {
    return(TRUE)
  } else if (is.integer(batch)) {
    return(TRUE)
  } else if (is.numeric(batch)) {
    if (batch %% 1 == 0) {
      return(TRUE)
    } else {
      stop("Error: Batch number must be object of type integer.\nYou provided a non-integer numeric value.")
    }
  } else {
    stop("Error: Batch number must be object of type integer.\nYou provided a non-integer value.")
  }
}
