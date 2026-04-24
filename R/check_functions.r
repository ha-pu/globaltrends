#' @title Validate input type
#'
#' @description
#' Checks that `input` satisfies the predicate `is.<type>()`. The predicate is
#' resolved at call time via [base::get0()], so any `is.*()` function visible
#' on the search path is valid. Throws an informative error using the original
#' argument name if the predicate returns `FALSE` or cannot be found.
#'
#' @param input Any R object to validate.
#' @param type Character scalar. Suffix of the `is.*()` predicate to apply
#'   (e.g. `"character"` resolves to `is.character()`). Must be non-`NA` and
#'   case-sensitive. Supported values include `"character"`, `"numeric"`,
#'   `"logical"`, `"list"`, and `"data.frame"`.
#'
#' @return Invisibly returns `TRUE` if `is.<type>(input)` is `TRUE`. Stops
#'   with an informative error if `type` is invalid, the predicate is not
#'   found, or `input` fails the type check.
#'
#' @keywords internal
#' @noRd

.check_input <- function(input, type, name = NULL) {
  name_input <- if (!is.null(name)) name else deparse(substitute(input))

  if (!is.character(type) || length(type) != 1L || is.na(type)) {
    stop(
      "Internal error: `type` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  pred_name <- paste0("is.", type)
  pred <- get0(pred_name, mode = "function")

  if (is.null(pred)) {
    stop(
      sprintf(
        "Internal error: Unknown type predicate '%s'. Provide a `type` value that has a corresponding `is.*()` function.",
        pred_name
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(pred(input))) {
    stop(
      sprintf(
        "Error: `%s` must be of type %s.\nYou provided an object of type %s.",
        name_input, type, typeof(input)
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @title Validate vector length
#'
#' @description
#' Checks that `length(input) <= max`. Most callers pass `max = 1` to enforce
#' scalar inputs; `max = 0` accepts only zero-length objects. Throws an
#' informative error using the original argument name if the check fails.
#'
#' @param input Any R object.
#' @param max Non-negative, finite, whole-number numeric scalar. Maximum
#'   allowed length. Accepts both `integer` and `double` (e.g. `1L` and `1`
#'   are both valid).
#'
#' @return Invisibly returns `TRUE` if `length(input) <= max`. Stops with an
#'   informative error if `max` is invalid or `input` exceeds the bound.
#'
#' @keywords internal
#' @noRd

.check_length <- function(input, max) {
  name_input <- deparse(substitute(input))

  if (
    !is.numeric(max) ||
      length(max) != 1 ||
      is.na(max) ||
      max < 0 ||
      max %% 1 != 0
  ) {
    stop(
      "Internal error: `max` must be a non-negative integer scalar.",
      call. = FALSE
    )
  }

  n <- length(input)
  if (n > max) {
    stop(
      sprintf(
        "Error: `%s` must have length <= %d.\nYou provided an object of length %d.",
        name_input, max, n
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @title Validate location-set name
#'
#' @description
#' Checks that `locations` is a non-`NA` character scalar. This is used for
#' arguments that name a location set (e.g. `"countries"`, `"us_states"`),
#' not a vector of individual location codes.
#'
#' @param locations Character scalar identifying a location set.
#'
#' @return Invisibly returns `TRUE` if valid. Stops if `locations` is not a
#'   length-1 non-`NA` character vector.
#'
#' @keywords internal
#' @noRd

.check_locations <- function(locations) {
  .check_length(locations, 1)
  .check_input(locations, "character")
  invisible(TRUE)
}

#' @title Validate batch identifier
#'
#' @description
#' Checks that `batch` is an integer-like scalar. Both `integer` vectors and
#' finite whole-number `double` values are accepted (e.g. `1L` and `1` are
#' both valid). `NULL` is passed through as valid to support optional batch
#' filters.
#'
#' @param batch `NULL`, an `integer` scalar, or a finite whole-number `double`
#'   scalar (e.g. `1`, `2`, `3`).
#'
#' @return Invisibly returns `TRUE` if valid. Stops if `batch` is non-scalar,
#'   non-numeric, non-finite, or has a fractional part.
#'
#' @keywords internal
#' @noRd

.check_batch <- function(batch) {
  if (is.null(batch)) {
    return(invisible(TRUE))
  }

  if (is.integer(batch)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(batch)) {
    stop(
      sprintf("Error: Batch id must be an integer value.\nYou provided an object of type %s.", typeof(batch)),
      call. = FALSE
    )
  }

  if (!all(is.finite(batch))) {
    stop("Error: Batch id must be a finite integer value.", call. = FALSE)
  }

  if (any(batch %% 1 != 0)) {
    stop(
      "Error: Batch id must be an integer value.\nYou provided a non-integer numeric value.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
