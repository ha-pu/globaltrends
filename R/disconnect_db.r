#' @title Disconnect from database
#'
#' @description
#' @details
#'
#' @seealso
#' @return
#' Message that disconnection was successful.
#'
#' @examples
#' \dontrun{
#' disconnect_db()
#' }
#' @export
#' @importFrom DBI dbDisconnect


disconnect_db <- function(db = globaltrends_db) {
  dbDisconnect(conn = db)
  message("Successfully disconnected.")
}
