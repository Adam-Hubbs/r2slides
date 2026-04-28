#' Convert a hex color code to RGB values


#' Convert inches to EMU's
#' @description
#' Google slides API's can only handle EMU or PT's. This function converts inches to EMU's so the user can continue to work in inches.
#'
#' @param x A numeric vector of measurements in inches.
#'
#' @returns
#' A numeric vector of measurements in english metric units (1/914,400 inches).
#' Errors if non-numeric input is provided.
#'
#' @keywords internal
in_to_emu <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort("Position or size argument must be numeric")
  }
  return(x * 914400)
}



recursively_replace <- function(x, what, with) {
  if (length(what) != 1) {
    cli::cli_abort("what must be a single value")
  }
  if (!is.character(what)) {
    cli::cli_abort("what must be a character string")
  }

  purrr::modify_tree(
    x,
    post = function(node) {
      if (is.list(node) && what %in% names(node)) {
        purrr::assign_in(node, what, with)
      } else {
        node
      }
    }
  )
}
