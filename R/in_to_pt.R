
#' Convert inches to points
#' @description
#' Google slides API's can only handle EMU or PT's. This function converts inches to PT's so the user can continue to work in inches.
#'
#' @param x A numeric vector of measurements in inches.
#'
#' @returns
#' A numeric vector of measurements in points (1/72 inches).
#' Errors if non-numeric input is provided.
#'
#' @keywords internal
in_to_pt <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort("Position or size argument must be numeric")
  }
  return(x * 72)
}
