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
  x * 914400
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

# Length of each string in UTF-16 code units, which is how the Slides API
# counts text indices. Characters outside the BMP (e.g. emoji) take two units.
utf16_length <- function(x) {
  purrr::map_int(x, \(s) sum(1L + (utf8ToInt(s) > 0xFFFF)))
}

# A style_rule selector that always returns the same 1-based inclusive range.
# A factory is used so `start`/`end` are captured by value.
range_selector <- function(start, end) {
  force(start)
  force(end)
  function() c(start, end)
}
