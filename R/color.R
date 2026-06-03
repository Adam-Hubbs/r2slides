theme_colors <- c(
  "DARK1",
  "LIGHT1",
  "DARK2",
  "LIGHT2",
  "ACCENT1",
  "ACCENT2",
  "ACCENT3",
  "ACCENT4",
  "ACCENT5",
  "ACCENT6",
  "HYPERLINK",
  "FOLLOWED_HYPERLINK",
  "TEXT1",
  "BACKGROUND1",
  "TEXT2",
  "BACKGROUND2"
)

# Parse any color input to list(red, green, blue) in [0, 1].
.parse_to_rgb <- function(x) {
  if (is.numeric(x)) {
    if (length(x) != 3 || any(x < 0) || any(x > 1)) {
      cli::cli_abort(
        "Numeric color must be {.code c(r, g, b)} with each value in [0, 1]."
      )
    }
    return(list(red = x[[1]], green = x[[2]], blue = x[[3]]))
  }
  if (!is.character(x) || length(x) != 1) {
    cli::cli_abort(
      "Color must be a length-1 character string or a numeric {.code c(r, g, b)} vector."
    )
  }
  rgb_int <- tryCatch(
    grDevices::col2rgb(x),
    error = function(e) {
      cli::cli_abort(
        "Invalid color {.val {x}}. Supply a hex string, a named R color, or an RGB vector.",
        call = rlang::caller_env(2)
      )
    }
  )
  list(
    red = unname(rgb_int[1L, 1L] / 255),
    green = unname(rgb_int[2L, 1L] / 255),
    blue = unname(rgb_int[3L, 1L] / 255)
  )
}

# Abstract base for all r2slides colors. Holds the optional alpha channel.
r2s_color <- S7::new_class(
  "r2s_color",
  abstract = TRUE,
  properties = list(
    alpha = S7::new_property(
      NULL | S7::class_numeric,
      validator = function(value) {
        if (is.null(value)) return(NULL)
        if (length(value) != 1) return("alpha must be a single value")
        if (value < 0 || value > 1) return("alpha must be in [0, 1]")
      }
    )
  )
)

#' Solid color
#'
#' A color with known RGB values. Accepts a hex string (`"#RRGGBB"` or
#' `"#RGB"`), a named R color (e.g. `"red"`), or an RGB numeric vector
#' `c(r, g, b)` with values in \[0, 1\].
#'
#' @param color Hex string, named R color, or `c(r, g, b)` in \[0, 1\].
#' @param alpha `NULL` or a number in \[0, 1\]. `NULL` (the default) omits the
#'   alpha field from fill API requests (treated as fully opaque). Only
#'   meaningful in fill contexts; ignored with a warning in text-style contexts.
#'
#' @export
solid_color <- S7::new_class(
  "solid_color",
  parent = r2s_color,
  properties = list(
    red = S7::class_numeric,
    green = S7::class_numeric,
    blue = S7::class_numeric
  ),
  constructor = function(color, alpha = NULL) {
    rgb <- .parse_to_rgb(color)
    S7::new_object(
      S7::S7_object(),
      red = rgb$red,
      green = rgb$green,
      blue = rgb$blue,
      alpha = alpha
    )
  }
)

#' Theme color
#'
#' A reference to a Google Slides presentation theme color. The actual RGB
#' value is not known until resolved against a specific presentation.
#'
#' @param theme One of `"DARK1"`, `"LIGHT1"`, `"DARK2"`, `"LIGHT2"`,
#'   `"ACCENT1"`-`"ACCENT6"`, `"HYPERLINK"`, `"FOLLOWED_HYPERLINK"`,
#'   `"TEXT1"`, `"BACKGROUND1"`, `"TEXT2"`, `"BACKGROUND2"`.
#' @param alpha `NULL` or a number in \[0, 1\]. Only meaningful in fill
#'   contexts; ignored with a warning in text-style contexts.
#'
#' @export
theme_color <- S7::new_class(
  "theme_color",
  parent = r2s_color,
  properties = list(
    theme = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1) return("theme must be a single value")
        if (!(value %in% theme_colors)) {
          return(paste0(
            "theme must be one of: ",
            paste(theme_colors, collapse = ", ")
          ))
        }
      }
    )
  ),
  constructor = function(theme, alpha = NULL) {
    S7::new_object(S7::S7_object(), theme = theme, alpha = alpha)
  }
)

# -- Helpers ------------------------------------------------------------------

# Internal: the innermost rgbColor/themeColor node for an r2s_color.
.color_inner_node <- function(color) {
  if (S7::S7_inherits(color, solid_color)) {
    list(
      rgbColor = list(red = color@red, green = color@green, blue = color@blue)
    )
  } else {
    list(themeColor = color@theme)
  }
}

# Convert any raw color value to an r2s_color. Used in property setters.
# @noRd
as_r2s_color <- function(value) {
  if (S7::S7_inherits(value, r2s_color)) return(value)
  if (is.character(value) && length(value) == 1 && value %in% theme_colors) {
    theme_color(value)
  } else {
    solid_color(value)
  }
}

# -- API functions ------------------------------------------------------------

#' Build the opaqueColor API node (text-style contexts).
#'
#' Warns when the color carries an alpha value, since `opaqueColor` does not
#' support transparency and the alpha will be dropped.
#'
#' @param color An [r2s_color] object.
#' @return `list(opaqueColor = ...)`.
#' @noRd
as_opaque_color_api <- function(color) {
  if (!is.null(color@alpha)) {
    cli::cli_warn(
      c(
        "Color has {.field alpha} set but {.code opaqueColor} does not support transparency.",
        "i" = "Alpha will be ignored. Use a fill context if transparency is needed."
      )
    )
  }
  list(opaqueColor = .color_inner_node(color))
}

#' Build the solidFill API node (fill contexts).
#'
#' @param color An [r2s_color] object.
#' @return `list(solidFill = ...)`.
#' @noRd
as_fill_api <- function(color) {
  fill <- list(color = .color_inner_node(color))
  if (!is.null(color@alpha)) fill$alpha <- color@alpha
  list(solidFill = fill)
}

# -- Format / print -----------------------------------------------------------

S7::method(format, solid_color) <- function(x, ...) {
  hex <- toupper(grDevices::rgb(x@red, x@green, x@blue))
  alpha_str <- if (!is.null(x@alpha)) paste0(" (alpha: ", x@alpha, ")") else ""
  paste0("<solid_color> ", hex, alpha_str)
}

S7::method(format, theme_color) <- function(x, ...) {
  alpha_str <- if (!is.null(x@alpha)) paste0(" (alpha: ", x@alpha, ")") else ""
  paste0("<theme_color> ", x@theme, alpha_str)
}

S7::method(print, solid_color) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

S7::method(print, theme_color) <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

# -- visualize ----------------------------------------------------------------

#' Visualize a color
#'
#' Renders a color swatch for [solid_color] objects, or prints an informative
#' message for [theme_color] objects whose actual value depends on the
#' presentation theme.
#'
#' @param color An [r2s_color] object.
#'
#' @export
visualize <- S7::new_generic("visualize", "color")

S7::method(visualize, solid_color) <- function(color) {
  hex <- grDevices::rgb(color@red, color@green, color@blue)
  op <- graphics::par(mar = rep(0, 4))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = hex, border = NA)
  invisible(color)
}

S7::method(visualize, theme_color) <- function(color) {
  cli::cli_inform(
    "Theme color {.val {color@theme}}: actual value depends on the presentation theme."
  )
  invisible(color)
}
