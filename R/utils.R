
#' Error message
#'
#' Custom error messaging method with clisymbols.
#'
#' @param msg Message for error
#'
#' @keywords internal
.error_message <- function(msg){
  error <- .message(
    clisymbols::symbol$cross,
    msg
  )
  stop(error, call. = FALSE)
}

#' Warning message
#'
#' Custom warning messaging method with clisymbols.
#'
#' @param msg Message for warning
#'
#' @keywords internal
.warning_message <- function(msg){
  error <- .message(
    clisymbols::symbol$circle_filled,
    msg
  )
  warning(error, call. = FALSE)
}


#' Message handler
#'
#' @param symbol Symbol prefix for message
#' @param msg Message content
#'
#' @keywords internal
.message <- function(symbol, msg){
  paste(
    symbol,
    msg,
    sep = " "
  )
}
