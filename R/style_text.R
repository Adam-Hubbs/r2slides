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

#' Normalize any user-supplied color to a canonical internal hex string.
#'
#' Accepts a hex string (`"#RRGGBB"` or `"#RGB"`), a named R color (e.g.
#' `"red"`, `"steelblue"`), an RGB numeric vector `c(r, g, b)` where each
#' component is in \[0, 1\], or a Google Slides theme color string (e.g.
#' `"ACCENT1"`).
#'
#' * Hex / named R colors are normalised to an uppercase 7-character hex string.
#' * Theme color strings are returned unchanged.
#' * `NULL` is returned as `NULL`.
#'
#' @param color A length-1 character (hex, named R color, or theme color), or
#'   a length-3 numeric vector `c(r, g, b)` in \[0, 1\].
#' @return A length-1 hex string, a theme color string, or `NULL`.
#' @noRd
normalize_color <- function(color) {
  if (is.null(color)) return(NULL)

  # Numeric c(r, g, b) in [0, 1] -> hex
  if (is.numeric(color)) {
    if (length(color) != 3 || any(color < 0) || any(color > 1)) {
      cli::cli_abort(
        "Numeric color must be {.code c(r, g, b)} with each value in [0, 1]."
      )
    }
    return(toupper(grDevices::rgb(color[1], color[2], color[3])))
  }

  if (!is.character(color) || length(color) != 1) {
    cli::cli_abort(
      "Color must be a length-1 character string or a numeric {.code c(r, g, b)} vector."
    )
  }

  # Theme color -> return as-is
  if (color %in% theme_colors) return(color)

  # Named R color or hex -> normalize to "#RRGGBB" via grDevices
  hex <- tryCatch(
    toupper(grDevices::rgb(t(grDevices::col2rgb(color)), maxColorValue = 255)),
    error = function(e) {
      cli::cli_abort(
        paste0(
          "Invalid color ", dQuote(color), ". ",
          "Supply a hex string, a named R color, or a theme color."
        ),
        call = rlang::caller_env(2)
      )
    }
  )
  hex
}

#' Validate a color property value for S7 objects.
#'
#' Returns an error string on failure (used by S7 property validators),
#' or `NULL` on success.
#'
#' @noRd
validate_color <- function(value) {
  if (is.null(value)) return(NULL)
  tryCatch(
    { normalize_color(value); NULL },
    error = function(e) conditionMessage(e)
  )
}

#' Convert an internally-stored color to the Google Slides API rgbColor node.
#'
#' Returns only the innermost color object — either `list(rgbColor = ...)` or
#' `list(themeColor = ...)`. Callers are responsible for wrapping this in the
#' appropriate parent node (`opaqueColor`, `solidFill`, etc.) required by the
#' specific API field being set.
#'
#' @param color A hex string or theme color string (as returned by
#'   [normalize_color()]), or `NULL`.
#' @return A named list with a single `rgbColor` or `themeColor` element, or
#'   `NULL`.
#' @noRd
color_to_rgb_api <- function(color) {
  if (is.null(color)) return(NULL)

  if (color %in% theme_colors) {
    return(list(themeColor = color))
  }

  # hex "#RRGGBB" -> fractional RGB for API
  rgb_int <- grDevices::col2rgb(color)
  list(
    rgbColor = list(
      red   = rgb_int[1L] / 255,
      green = rgb_int[2L] / 255,
      blue  = rgb_int[3L] / 255
    )
  )
}

#' Wrap a color as an `opaqueColor` node (used by updateTextStyle).
#' @noRd
color_to_opaque_color <- function(color) {
  inner <- color_to_rgb_api(color)
  if (is.null(inner)) NULL else list(opaqueColor = inner)
}

#' Wrap a color as a `solidFill` node (used by updateTableCellProperties).
#' @noRd
color_to_solid_fill <- function(color) {
  inner <- color_to_rgb_api(color)
  if (is.null(inner)) NULL else list(solidFill = list(color = inner))
}

#' Text styling properties
#'
#' @param bg_color Background color. Accepts a hex string (e.g. `"#FF0000"`),
#'   a named R color (e.g. `"red"`), an RGB numeric vector `c(r, g, b)` with
#'   values in \[0, 1\], or a theme color string (e.g. `"ACCENT1"`).
#' @param text_color Text (foreground) color. Same formats as `bg_color`.
#' @param bold A logical indicating whether the text should be bold.
#' @param italic A logical indicating whether the text should be italic.
#' @param font_family A string specifying the font family.
#' @param font_size A numeric specifying the font size in points.
#' @param link A string specifying the link URL.
#' @param baseline_offset A numeric specifying the baseline offset in points.
#' @param small_caps A logical indicating whether the text should be in small caps.
#' @param strikethrough A logical indicating whether the text should be strikethrough.
#' @param underline A logical indicating whether the text should be underlined.
#'
#' @export
text_style <- S7::new_class(
  "text_style",
  properties = list(
    bg_color = S7::new_property(
      setter = function(self, value) {
        self@bg_color <- normalize_color(value)
        self
      },
      validator = validate_color
    ),
    text_color = S7::new_property(
      setter = function(self, value) {
        self@text_color <- normalize_color(value)
        self
      },
      validator = validate_color
    ),
    bold = S7::new_property(
      NULL | S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("bold must be a single value")
          }
        }
      }
    ),
    italic = S7::new_property(
      NULL | S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("italic must be a single value")
          }
        }
      }
    ),
    font_family = S7::new_property(
      NULL | S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("font_family must be a single value")
          }
          if (value == "") {
            return("font_family can't be an empty string")
          }
        }
      }
    ),
    font_size = S7::new_property(
      NULL | S7::class_double,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("font_size must be a single value")
          }
          if (value <= 0) return("font_size must be greater than 0")
        }
      }
    ),
    link = S7::new_property(
      NULL | S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          # Here I restrict to just url link, the API also supports relative links to other slides in the presentation. Come back to this for that functionality later
          if (length(value) != 1) {
            return("link must be a single value")
          }
          if (value == "") {
            return("link can't be an empty string")
          }
          if (!grepl("^https?://", value)) {
            return("link must be a url")
          }
        }
      }
    ),
    baseline_offset = S7::new_property(
      NULL | S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("baseline_offset must be a single value")
          }
          if (value != "SUPERSCRIPT" && value != "SUBSCRIPT") {
            return(
              "baseline_offset must be either 'SUPERSCRIPT' or 'SUBSCRIPT'"
            )
          }
        }
      }
    ),
    small_caps = S7::new_property(
      NULL | S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("small_caps must be a single value")
          }
        }
      }
    ),
    strikethrough = S7::new_property(
      NULL | S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("strikethrough must be a single value")
          }
        }
      }
    ),
    underline = S7::new_property(
      NULL | S7::class_logical,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("underline must be a single value")
          }
        }
      }
    ),

    # Computed properties
    style = S7::new_property(S7::class_list, getter = function(self) {
      list(
        backgroundColor = color_to_opaque_color(self@bg_color),
        foregroundColor = color_to_opaque_color(self@text_color),
        bold            = self@bold,
        italic          = self@italic,
        fontFamily      = self@font_family,
        fontSize        = if (!is.null(self@font_size)) list(magnitude = self@font_size, unit = "PT") else NULL,
        link            = if (!is.null(self@link)) list(url = self@link) else NULL,
        baselineOffset  = self@baseline_offset,
        smallCaps       = self@small_caps,
        strikethrough   = self@strikethrough,
        underline       = self@underline
      ) |> purrr::compact()
    }),
    fields = S7::new_property(S7::class_character, getter = function(self) {
      f <- character()

      if (!is.null(self@bg_color)) {
        f <- c(f, "backgroundColor")
      }
      if (!is.null(self@text_color)) {
        f <- c(f, "foregroundColor")
      }
      if (!is.null(self@bold)) {
        f <- c(f, "bold")
      }
      if (!is.null(self@italic)) {
        f <- c(f, "italic")
      }
      if (!is.null(self@font_family)) {
        f <- c(f, "fontFamily")
      }
      if (!is.null(self@font_size)) {
        f <- c(f, "fontSize")
      }
      if (!is.null(self@link)) {
        f <- c(f, "link")
      }
      if (!is.null(self@baseline_offset)) {
        f <- c(f, "baselineOffset")
      }
      if (!is.null(self@small_caps)) {
        f <- c(f, "smallCaps")
      }
      if (!is.null(self@strikethrough)) {
        f <- c(f, "strikethrough")
      }
      if (!is.null(self@underline)) {
        f <- c(f, "underline")
      }

      f
    })
  )
)


# Register print method
S7::method(print, text_style) <- function(x, ...) {
  cli::cli_text("{.strong Text Style Object}")
  print(x@style)
}


# Currently no validation for the function. It should return either TRUE/FALSE (And be applied to the whole string), or return a vector of START_INDEX, END_INDEX

#' Style Rule Object
#'
#' A style rule object is a list of text styles that are applied to a set of selectors.
#' @param when A list of selection functions that determine when to apply each style.
#' Each function should return either TRUE/FALSE (to apply to the whole text) or a vector of START_INDEX, END_INDEX (to apply to a specific part of the text).
#' @param what A list of text styles. Each style corresponds to a selector in `when`.
#' If the number of styles is less than the number of selectors, the last style is treated as the default style to apply when no other selectors match.
#' @param default An optional default text style to apply when no selectors match.
#'
#' @export
style_rule <- S7::new_class(
  'style_rule',
  properties = list(
    selector = S7::new_property(S7::class_any), # Should be a quosure
    style = S7::new_property(
      text_style | S7::class_vector
    ), # text_style object or a vector of text_style objects
    num_selectors = S7::new_property(getter = function(self) {
      if (is.function(self@selector)) {
        1L
      } else {
        length(self@selector)
      }
    }),
    num_styles = S7::new_property(getter = function(self) {
      length(self@style)
    }),
    has_default_style = S7::new_property(
      S7::class_logical,
      getter = function(self) {
        if (self@num_selectors != self@num_styles) {
          #mismatch between number of selectors and number of styles
          TRUE
        } else {
          FALSE
        }
      }
    )
  ),
  # Assume when and what are lists of quos
  constructor = function(when = TRUE, what = NULL, default = NULL) {
    if (is.null(what) && is.null(default)) {
      cli::cli_abort(
        "Must provide either `what` or `default`",
        call = rlang::caller_env()
      )
    }
    when_quos <- rlang::enquos(when)

    # If when was a vector/list, we need to extract each element as a separate quosure
    # Check if we got a single quosure that contains c(...)
    if (length(when_quos) == 1) {
      when_expr <- rlang::quo_get_expr(when_quos[[1]])

      # Check if it's a c() call
      if (
        rlang::is_call(when_expr) &&
          (rlang::call_name(when_expr) == "c" ||
            rlang::call_name(when_expr) == "list")
      ) {
        # Extract each argument from c() or list() as separate quosures
        when_env <- rlang::quo_get_env(when_quos[[1]])
        when_quos <- purrr::map(
          rlang::call_args(when_expr),
          ~ rlang::new_quosure(.x, when_env)
        )
      }
    }

    styles <- purrr::compact(c(what))
    n_selectors <- length(when_quos)
    n_styles <- length(styles)

    # What provided but n_styles != n_selectors
    if (!is.null(what) && n_styles != n_selectors) {
      cli::cli_abort(
        c(
          "Number of selectors and styles must match.",
          "i" = "{n_selectors} selector{?s} supplied but {n_styles} style{?s} in {.arg what}.",
          "i" = "Use {.arg default} to supply a fallback style applied when no selector matches."
        ),
        call = rlang::caller_env()
      )
    }

    # n_selectors > 1 but no what (If it == 1, then default is used)
    if (is.null(what) && n_selectors > 1) {
      # Default exists (check at top of func)
      cli::cli_abort(
        c(
          "Multiple selectors supplied but no {.arg what}.",
          "i" = "Provide a style in {.arg what} for each selector, or use a single {.arg default}."
        ),
        call = rlang::caller_env()
      )
    }

    # explicit selector with default but no what
    if (is.null(what) && n_selectors == 1) {
      when_expr <- rlang::quo_get_expr(when_quos[[1]])
      if (!isTRUE(when_expr)) {
        cli::cli_abort(
          c(
            "A selector was provided with {.arg default} but no {.arg what}.",
            "i" = "Provide a style in {.arg what} for each selector, or remove {.arg when} to use {.arg default} alone."
          ),
          call = rlang::caller_env()
        )
      }
    }

    if (!is.null(default)) {
      styles <- c(what, default)
    }

    styles <- styles |> purrr::compact()

    new_object(
      S7_object(),
      selector = when_quos,
      style = styles
    )
  }
)


#' Create a styling request
#'
#' @param style_rule A style_rule object
#' @param text A character string of text
#' @param element_id A string ID of an existing text element to update. If element_id is `NULL` a new element will be created.
#' @param ... Data to be available in the data mask where the selector function will be executed. Must be named.
#' @param call Optional. Call environment used in error messages.
#'
#' @returns A list of Google Slides styling requests
#'
#' @export
create_styling_request <- function(
  style_rule,
  text,
  element_id,
  ...,
  call = rlang::caller_env()
) {
  style_requests <- c()

  extra_vars <- rlang::list2(...)

  # check that extra_vars are named
  if (!rlang::is_named2(extra_vars)) {
    cli::cli_abort(
      "Extra variables supplied to create_styling_request must be named.",
      call = call
    )
  }

  # Create data mask
  #dm <- rlang::as_data_mask(c(list(text = text), extra_vars))
  dm <- c(text = text, extra_vars)

  # Do any of the selector functions evaluate to true or any non-zero indices?
  when_true <- FALSE

  for (rule in 1:style_rule@num_selectors) {
    tryCatch(
      {
        quo_in_dm <- rlang::eval_tidy(
          expr = style_rule@selector[[rule]],
          data = dm
        )
        #If the output is a bare expression (TRUE/FALSE, vector of index's, etc.) then return it, else evaluate the function returned
        if (is.function(quo_in_dm)) {
          f_output <- quo_in_dm()
        } else {
          f_output <- quo_in_dm
        }
      },
      error = function(error) {
        error_msg <- conditionMessage(error)

        # Get the selector function expression for the error message
        selector_expr <- rlang::as_label(style_rule@selector[[rule]])

        # Check if this is an "object not found" error
        if (grepl("^object '.+' not found$", error_msg)) {
          # Extract the missing variable name
          # missing function argument (new branch)
          missing_var <- sub("^object '(.+)' not found$", "\\1", error_msg)

          cli::cli_abort(
            c(
              "x" = "Variable {.var {missing_var}} not found when evaluating selector function {.code {selector_expr}}.",
              "i" = "Add {.var {missing_var}} to the {.fn add_text} call to make it available to the selector function."
            ),
            call = call
          )
        } else if (grepl("^argument [\"'](.+)[\"'] is missing", error_msg)) {
          missing_var <- sub(
            "^argument [\"'](.+)[\"'] is missing.*$",
            "\\1",
            error_msg
          )

          cli::cli_abort(
            c(
              "x" = "Variable {.var {missing_var}} not found when evaluating selector function {.code {selector_expr}}.",
              "i" = "Add {.var {missing_var}} to the {.fn add_text} call to make it available to the selector function."
            ),
            call = call
          )
        } else {
          # For other errors, re-throw with context
          cli::cli_abort(
            c(
              "x" = "Error evaluating selector function {.code {selector_expr}}."
            ),
            parent = error,
            call = call
          )
        }
      }
    )

    if (is.logical(f_output)) {
      if (is.na(f_output)) {
        cli::cli_warn(
          c(
            "i" = "Selector function returned {.code NA}. Not preforming any styling."
          )
        )
        selection_index <- NULL
      } else if (f_output) {
        when_true <- TRUE
        selection_index <- c(1, nchar(text))
      } else {
        selection_index <- NULL
      }
    } else if (is.numeric(f_output) && length(f_output) == 2) {
      # If returning c(0, 0) (No match found) then return NULL so no styling request will be created
      if (f_output[1] == 0 && f_output[2] == 0) {
        selection_index <- NULL
      } else {
        when_true <- TRUE
        # Convert to 0-based indices for Google Slides API
        selection_index <- f_output
      }
    } else {
      cli::cli_abort(
        "Internal Error. Please contact the package developers if you ever see this error.",
        call = call
      )
    }

    if (!is.null(selection_index)) {
      # Convert 1-based R indices to 0-based Google Slides API indices
      start_index <- as.integer(selection_index[1] - 1)
      end_index <- as.integer(selection_index[2])

      # Create request
      style_requests <- append(
        style_requests,
        list(
          updateTextStyle = list(
            objectId = element_id,
            textRange = list(
              type = "FIXED_RANGE",
              startIndex = start_index,
              endIndex = end_index
            ),
            style = style_rule@style[[rule]]@style,
            fields = paste(
              style_rule@style[[rule]]@fields,
              collapse = ","
            )
          )
        )
      )
    }
  }

  if (!when_true && style_rule@has_default_style) {
    style_requests <- append(
      style_requests,
      list(
        updateTextStyle = list(
          objectId = element_id,
          textRange = list(
            type = "FIXED_RANGE",
            startIndex = 0,
            endIndex = nchar(text)
          ),
          style = style_rule@style[[style_rule@num_styles]]@style,
          fields = paste(
            style_rule@style[[style_rule@num_styles]]@fields,
            collapse = ","
          )
        )
      )
    )
  }

  return(style_requests)
}


print_selector <- function(selector_list) {
  purrr::map(selector_list, function(element) {
    if (rlang::is_quosure(element)) {
      # should always be true, if it is not something broke
      inner_expr <- rlang::quo_get_expr(element)
      if (rlang::is_quosure(inner_expr)) {
        # If the expression inside is also a quosure, unwrap it too
        rlang::quo_get_expr(inner_expr)
      } else {
        inner_expr
      }
    } else {
      element
    }
  })
}

# Register print method
S7::method(print, style_rule) <- function(x, ...) {
  cli::cli_text("{.strong Style Rule Object}")
  print_selector(x@selector) |> print()
  print(x@style)
}


# In selection functions, text is currently evaluated in the data mask so it maps onto the text of the text box.
# If you want to conditionally format on a variable that is not the text in the text box, use something else rather than 'text'. This is only supported when supplying predicate functions (which evaluate to TRUE or FALSE)
# or use the .env pronoun

S7::method(`+`, list(style_rule, style_rule)) <- function(e1, e2) {
  new_default_style <- NULL

  if (e1@has_default_style) {
    if (e2@has_default_style) {
      new_default_style <- e1@style[[e1@num_styles]] + e2@style[[e2@num_styles]] # Put the default styles together
    } else {
      # Only e1 has default style
      new_default_style <- e1@style[[e1@num_styles]]
    }
  } else {
    if (e2@has_default_style) {
      # only e2 has default style
      new_default_style <- e2@style[[e2@num_styles]]
    }
  }

  # TODO: More logic around if the selection functions are the same and the styles are compatible
  if (!is.null(new_default_style)) {
    style_rule(
      when = c(!!!e1@selector, !!!e2@selector), # We may need to unwrap these
      what = c(
        e1@style[seq_len(e1@num_selectors)],
        e2@style[seq_len(e2@num_selectors)]
      ),
      default = new_default_style
    )
  } else {
    style_rule(
      when = c(!!!e1@selector, !!!e2@selector),
      what = c(
        e1@style[seq_len(e1@num_selectors)],
        e2@style[seq_len(e2@num_selectors)]
      )
    )
  }
}


S7::method(`+`, list(text_style, text_style)) <- function(e1, e2) {
  combine_style_impl(e1, e2)
}

#' @export
#' @rdname combine_style
#' @title Combine Text Styles
#' @description Combine text styles in a priority order
#' @param ... Text styles to combine
#' @param .priority Priority order in which to combine styles
#' @return A text_style object
combine_style <- function(..., .priority = c('first', 'last', 'error')) {
  .priority <- rlang::arg_match(.priority)

  # Check that all arguments passed in ... are text_style objects
  if (
    !all(purrr::map_lgl(list(...), \(x) inherits(x, "r2slides::text_style")))
  ) {
    cli::cli_abort(
      c(
        "x" = "All arguments passed in ... must be text_style objects"
      )
    )
  }

  call <- rlang::current_env()

  if (.priority == 'first') {
    purrr::reduce(list(...), \(x, y) {
      combine_style_impl(x, y, error_on_contradiction = FALSE, call = call)
    })
  } else if (.priority == 'last') {
    purrr::reduce(
      rev(list(...)),
      \(x, y) {
        combine_style_impl(x, y, error_on_contradiction = FALSE, call = call)
      }
    )
  } else {
    purrr::reduce(list(...), \(x, y) {
      combine_style_impl(x, y, error_on_contradiction = TRUE, call = call)
    })
  }
}


combine_style_impl <- function(
  e1,
  e2,
  error_on_contradiction = TRUE,
  call = rlang::caller_env()
) {
  if (error_on_contradiction) {
    # Get the fields from both styles
    fields1 <- e1@fields
    fields2 <- e2@fields

    # Check for overlapping fields
    overlapping_fields <- intersect(fields1, fields2)

    if (length(overlapping_fields) > 0) {
      # Check if the overlapping fields have the same values
      for (field in overlapping_fields) {
        # Get values directly from properties
        val1 <- switch(
          field,
          "backgroundColor" = e1@bg_color,
          "foregroundColor" = e1@text_color,
          "bold" = e1@bold,
          "italic" = e1@italic,
          "fontFamily" = e1@font_family,
          "fontSize" = e1@font_size,
          "link" = e1@link,
          "baselineOffset" = e1@baseline_offset,
          "smallCaps" = e1@small_caps,
          "strikethrough" = e1@strikethrough,
          "underline" = e1@underline
        )

        val2 <- switch(
          field,
          "backgroundColor" = e2@bg_color,
          "foregroundColor" = e2@text_color,
          "bold" = e2@bold,
          "italic" = e2@italic,
          "fontFamily" = e2@font_family,
          "fontSize" = e2@font_size,
          "link" = e2@link,
          "baselineOffset" = e2@baseline_offset,
          "smallCaps" = e2@small_caps,
          "strikethrough" = e2@strikethrough,
          "underline" = e2@underline
        )

        # Check if values are identical
        if (!identical(val1, val2)) {
          cli::cli_abort(
            c(
              "Can't combine text styles: conflicting values for {.field {field}}",
              "x" = "Object 1 has {.val {val1}}",
              "x" = "Object 2 has {.val {val2}}"
            ),
            call = call
          )
        }
      }
    }
  }
  # If we get here, either no overlaps or all overlaps have identical values
  # Create new text_style with combined properties
  mush_styles(e1, e2)
}

mush_styles <- function(e1, e2) {
  text_style(
    bg_color = e1@bg_color %||% e2@bg_color,
    text_color = e1@text_color %||% e2@text_color,
    bold = e1@bold %||% e2@bold,
    italic = e1@italic %||% e2@italic,
    font_family = e1@font_family %||% e2@font_family,
    font_size = e1@font_size %||% e2@font_size,
    link = e1@link %||% e2@link,
    baseline_offset = e1@baseline_offset %||% e2@baseline_offset,
    small_caps = e1@small_caps %||% e2@small_caps,
    strikethrough = e1@strikethrough %||% e2@strikethrough,
    underline = e1@underline %||% e2@underline
  )
}
