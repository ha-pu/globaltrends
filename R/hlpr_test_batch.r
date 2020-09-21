#' @title Test batch number
#'
#' @keywords internal

.test_batch <- function(batch) {
  if(is.null(batch)) {
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
