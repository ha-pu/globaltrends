#' @title Validate input type
#'
#' @description
#' Internal helper to validate the base type of an argument. The function is
#' intentionally lightweight and used throughout the package for early, clear
#' input validation.
#'
#' @param input Any R object to validate.
#' @param type Character scalar. Expected base type. Supported values are those
#'   with corresponding `is.*()` predicates (e.g., `"character"`, `"numeric"`,
#'   `"logical"`, `"list"`, `"data.frame"`).
#'
#' @return Invisibly returns `TRUE` if `input` matches the expected type.
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang as_name enquo

.check_input <- function(input, type) {
  # Capture the argument name for high-quality error messages.
  name_input <- as_name(enquo(input))

  .check_length(type, 1)
  if (!is.character(type) || is.na(type)) {
    stop(
      "Internal error: `type` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  pred_name <- paste0("is.", type)
  pred <- get0(pred_name, mode = "function")

  if (is.null(pred)) {
    stop(
      paste0(
        "Internal error: Unknown type predicate '",
        pred_name,
        "'. ",
        "Provide a `type` value that has a corresponding `is.*()` function."
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(pred(input))) {
    stop(
      paste0(
        "Error: `",
        name_input,
        "` must be of type ",
        type,
        ".\n",
        "You provided an object of type ",
        typeof(input),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @title Validate vector length
#'
#' @description
#' Internal helper to validate that `input` has length within a specified bound.
#' Most callers use it to enforce scalar inputs (`max = 1`), but the function
#' supports arbitrary upper bounds.
#'
#' @param input Any R object with a defined length.
#' @param max Integer scalar. Maximum allowed length (must be >= 0).
#'
#' @return Invisibly returns `TRUE` if `length(input) <= max`.
#'
#' @keywords internal
#' @noRd
#' @importFrom rlang as_name enquo

.check_length <- function(input, max) {
  name_input <- as_name(enquo(input))

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
      paste0(
        "Error: `",
        name_input,
        "` must have length <= ",
        max,
        ".\n",
        "You provided an object of length ",
        n,
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @title Validate location-set name input
#'
#' @description
#' Validates that `locations` is a character scalar. This helper is used for
#' arguments that refer to a named location set (e.g., `"countries"`,
#' `"us_states"`), rather than a vector of location codes.
#'
#' @param locations Character scalar.
#'
#' @return Invisibly returns `TRUE` if valid.
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
#' Validates that a batch identifier is an integer-like scalar. The package
#' treats batches as integer ids. Numeric inputs are accepted only if they
#' represent whole numbers (e.g., `1`, `2`, `3`) and are finite.
#'
#' `NULL` is accepted and returned as valid, to support optional batch filters.
#'
#' @param batch Integer scalar, numeric scalar representing an integer, or `NULL`.
#'
#' @return Invisibly returns `TRUE` if valid.
#'
#' @keywords internal
#' @noRd

.check_batch <- function(batch) {
  if (is.null(batch)) {
    return(invisible(TRUE))
  }

  if (length(batch) != 1) {
    stop("Error: Batch id must be a scalar (length 1).", call. = FALSE)
  }

  if (is.integer(batch)) {
    return(invisible(TRUE))
  }

  if (is.numeric(batch)) {
    if (!is.finite(batch)) {
      stop("Error: Batch id must be a finite integer value.", call. = FALSE)
    }
    if (batch %% 1 != 0) {
      stop(
        "Error: Batch id must be an integer value.\nYou provided a non-integer numeric value.",
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }

  stop(
    paste0(
      "Error: Batch id must be an integer value.\n",
      "You provided an object of type ",
      typeof(batch),
      "."
    ),
    call. = FALSE
  )
}
