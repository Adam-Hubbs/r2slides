#' Convert a hex color code to RGB values
#'
#' @param hex_color A single string representing a 6-digit hex color code, with or without a leading "#".
#'
#' @returns
#' A list with three components: `red`, `green`, and `blue`, each containing a number between 0 and 1.
#' Will error if the input is not a valid 6-digit hex color code.
#'
#' @keywords internal
hex_to_rgb <- function(hex_color) {
  #Remove `#`
  hex_color <- gsub("^#", "", hex_color)

  # Check if valid hex color
  if (!grepl("^[0-9A-Fa-f]{6}$", hex_color)) {
    cli::cli_abort("{.var hex_color} must be a valid 6-digit hex color code")
  }

  # Extract the red, green, and blue components
  red <- strtoi(substr(hex_color, 1, 2), base = 16) / 255
  green <- strtoi(substr(hex_color, 3, 4), base = 16) / 255
  blue <- strtoi(substr(hex_color, 5, 6), base = 16) / 255

  # Return as a list
  return(list(
    red = red,
    green = green,
    blue = blue
  ))
}


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

