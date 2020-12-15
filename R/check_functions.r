#' @title Check confidence interval
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom glue glue

.check_ci <- function(ci) {
  if (length(ci) > 1) stop(glue("Error: 'ci' must be object of length 1.\nYou provided an object of length {length(ci)}."))
  if (!is.double(ci)) stop(glue("Error: 'ci' must be object of type double.\nYou supplied an object of type {typeof(ci)}."))
  if (ci <= 0 | ci >= 1) stop(glue("Error: 'ci' must be greater than 0 and less than 1.\nYou supplied {ci}."))
}

#' @title Check measure
#'
#' @keywords internal
#' @noRd

.check_measure <- function(measure) {
  if (!is.null(measure) & !is.character(measure)) stop(glue("Error: 'measure' must be object of type character.\nYou supplied an object of type {typeof(measure)}."))
  if (length(measure) > 1) stop(glue("Error: 'measure' must be object of length 1.\nYou provided an object of length {length(measure)}."))
  if (!(measure %in% c("gini", "hhi", "entropy"))) stop(glue("Error: 'measure' must be either 'gini', 'hhi', or 'entropy'.\nYou supplied {measure}."))
}

#' @title Check type
#'
#' @keywords internal
#' @noRd

.check_type <- function(type) {
  if (length(type) > 1) stop(glue("Error: 'type' must be object of length 1.\nYou provided an object of length {length(type)}."))
  if (!is.character(type)) stop(glue("Error: 'type' must be object of type character.\nYou supplied an object of type {typeof(type)}."))
  if (!(type %in% c("obs", "sad", "trd"))) stop(glue("Error: 'type' must be either 'obs', 'sad', or 'trd'.\nYou supplied {type}."))
}

#' @title Check smooth
#'
#' @keywords internal
#' @noRd

.check_smooth <- function(smooth) {
  if (length(smooth) > 1) stop(glue("Error: 'smooth' must be object of length 1.\nYou provided an object of length {length(smooth)}."))
  if (!is.logical(smooth)) stop(glue("Error: 'smooth' must be of type 'logical'.\nYou supplied an object of type {typeof(smooth)}."))
}

#' @title Check locations
#'
#' @keywords internal
#' @noRd

.check_locations <- function(locations) {
  if (length(locations) > 1) stop(glue("Error: 'locations' must be object of length 1.\nYou provided an object of length {length(locations)}."))
  if (!is.character(locations)) stop(glue("Error: 'locations' must be of type 'character'.\nYou supplied an object of type {typeof(locations)}."))
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
      stop("Error: Batch number must be an integer value.\nYou supplied a non-integer numeric value.")
    }
  } else {
    stop("Error: Batch number must be an integer value.\nYou supplied a non-integer value.")
  }
}
