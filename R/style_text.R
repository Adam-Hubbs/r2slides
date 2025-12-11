style_text_object <- function(
  element_id,
  bg_color,
  font_color,
  bold,
  italic,
  font_family,
  font_size,
  link,
  baseline_offset,
  small_caps,
  strikethrough,
  underline
) {}


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


validate_color <- function(value) {
  if (is.null(value)) {
    return()
  }
  if (length(value) == 3) {
    # Validate each value is length 1 numeric between 0 to 1.0
    if (any(!is.numeric(value))) {
      "must be an rgb color in the form of c(r, g, b) where r, g, b are numeric between 0 and 1 or a theme color"
    }
    if (any(value < 0 | value > 1)) {
      "must be an rgb color in the form of c(r, g, b) where r, g, b are numeric between 0 and 1 or a theme color"
    }
  } else if (length(value) == 1) {
    if (!(value %in% theme_colors)) {
      "must be an rgb color in the form of c(r, g, b) where r, g, b are numeric between 0 and 1 or a theme color"
    }
  } else {
    "must be an rgb color in the form of c(r, g, b) where r, g, b are numeric between 0 and 1 or a theme color"
  }
}


text_style <- S7::new_class(
  "text_style",
  properties = list(
    bg_color = S7::new_property(validator = validate_color),
    text_color = S7::new_property(validator = validate_color),
    bold = S7::new_property(S7::class_logical, validator = function(value) {
      if (!is.null(value)) {
        if (length(value) != 1) {
          "bold must be a single value"
        }
      }
    }),
    italic = S7::new_property(S7::class_logical, validator = function(value) {
      if (!is.null(value)) {
        if (length(value) != 1) {
          "italic must be a single value"
        }
      }
    }),
    font_family = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "font_family must be a single value"
          }
          if (value == "") {
            "font_family can't be an empty string"
          }
        }
      }
    ),
    font_size = S7::new_property(S7::class_double, validator = function(value) {
      if (!is.null(value)) {
        if (length(value) != 1) {
          "font_size must be a single value"
        }
        if (value <= 0) "font_size must be greater than 0"
      }
    }),
    link = S7::new_property(S7::class_character, validator = function(value) {
      if (!is.null(value)) {
        # Here I restrict to just url link, the API also supports relative links to other slides in the presentation. Come back to this for that functionality later
        if (length(value) != 1) {
          "link must be a single value"
        }
        if (value == "") {
          "link can't be an empty string"
        }
        if (!grepl("^https?://", value)) {
          "link must be a url"
        }
      }
    }),
    baseline_offset = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "baseline_offset must be a single value"
          }
          if (value != "SUPERSCRIPT" && value != "SUBSCRIPT") {
            "baseline_offset must be either 'SUPERSCRIPT' or 'SUBSCRIPT'"
          }
        }
      }
    ),
    small_caps = S7::new_property(
      S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "small_caps must be a single value"
          }
        }
      }
    ),
    strikethrough = S7::new_property(
      S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "strikethrough must be a single value"
          }
        }
      }
    ),
    underline = S7::new_property(
      S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            "underline must be a single value"
          }
        }
      }
    ),

    # Computed properties
    style = S7::new_property(S7::class_list, getter = function(self) {
      # bg_color
      if (!is.null(self@bg_color)) {
        if (length(self@bg_color) == 3) {
          backgroundColor <- list(
            opaqueColor = list(
              color = list(
                rgbColor = self@bg_color
              )
            )
          )
        } else {
          backgroundColor <- list(
            opaqueColor = list(
              color = list(
                themeColor = self@bg_color
              )
            )
          )
        }
      } else {
        backgroundColor <- NULL
      }

      # text_color
      if (!is.null(self@text_color)) {
        if (length(self@text_color) == 3) {
          foregroundColor <- list(
            opaqueColor = list(
              color = list(
                rgbColor = self@text_color
              )
            )
          )
        } else {
          foregroundColor <- list(
            opaqueColor = list(
              color = list(
                themeColor = self@text_color
              )
            )
          )
        }
      } else {
        foregroundColor <- NULL
      }

      # bold, italic - no manipulation

      # font_family
      if (!is.null(self@font_family)) {
        fontFamily <- self@font_family
      } else {
        fontFamily <- NULL
      }

      #font_size
      if (!is.null(self@font_size)) {
        fontSize <- list(
          magnitude = self@font_size,
          unit = "PT"
        )
      } else {
        fontSize <- NULL
      }

      #link
      if (!is.null(self@link)) {
        link <- list(
          url = self@link
        )
      } else {
        link <- NULL
      }

      #baseline_offset
      if (!is.null(self@baseline_offset)) {
        baselineOffset <- self@baseline_offset
      } else {
        baselineOffset <- NULL
      }

      #small_caps, strikethrough, underline - no manipulation

      # Build request
      list(
        backgroundColor = backgroundColor,
        foregroundColor = foregroundColor,
        bold = self@bold,
        italic = self@italic,
        fontFamily = fontFamily,
        fontSize = fontSize,
        link = link,
        baselineOffset = baselineOffset,
        smallCaps = self@small_caps,
        strikethrough = self@strikethrough,
        underline = self@underline
      ) |>
        purrr::compact()
    })
  )
)


# Register print method
S7::method(print, text_style) <- function(x, ...) {
  cli::cli_div(
    theme = list(span.val = list(color = "blue", "font-weight" = "bold"))
  )

  cli::cli_text("{.strong Text Style Object}")
  print(x@style)
}
