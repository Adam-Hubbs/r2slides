v_align_values <- c("VERTICAL_ALIGNMENT_UNSPECIFIED", "TOP", "MIDDLE", "BOTTOM")

ft_dash_style_map <- c(
  "solid"  = "SOLID",
  "dashed" = "DASH",
  "dotted" = "DOT",
  "none"   = "TRANSPARENT"
)

# Internal helper: build the border side list for one cell border slot.
# Returns NULL when all inputs are NULL.
# @noRd
make_border_side <- function(color, width, dash_style) {
  if (is.null(color) && is.null(width) && is.null(dash_style)) return(NULL)
  list(color = color, width = width, dash_style = dash_style)
}

#' @noRd
cell_style <- S7::new_class(
  "cell_style",
  properties = list(
    bg_color = S7::new_property(
      setter = function(self, value) {
        self@bg_color <- normalize_color(value)
        self
      },
      validator = validate_color
    ),

    text_style = S7::new_property(
      NULL | text_style | style_rule
    ),

    v_align = S7::new_property(
      NULL | S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) return("v_align must be a single value")
          if (!(value %in% v_align_values)) {
            return(paste0(
              "v_align must be one of: ",
              paste(v_align_values, collapse = ", ")
            ))
          }
        }
      }
    ),

    col_span = S7::new_property(
      NULL | S7::class_integer,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) return("col_span must be a single value")
          if (value < 1L)         return("col_span must be a positive integer")
        }
      }
    ),

    row_span = S7::new_property(
      NULL | S7::class_integer,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) return("row_span must be a single value")
          if (value < 1L)         return("row_span must be a positive integer")
        }
      }
    ),

    # Each border slot is NULL or a list(color, width, dash_style).
    # Use make_border_side() to construct.
    border_top    = S7::new_property(NULL | S7::class_list),
    border_bottom = S7::new_property(NULL | S7::class_list),
    border_left   = S7::new_property(NULL | S7::class_list),
    border_right  = S7::new_property(NULL | S7::class_list)
  )
)

#' @noRd
table_cell <- S7::new_class(
  "table_cell",
  properties = list(
    row_index = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) return("row_index must be a single value")
        if (value < 0L)         return("row_index must be >= 0")
      }
    ),

    col_index = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) return("col_index must be a single value")
        if (value < 0L)         return("col_index must be >= 0")
      }
    ),

    text  = S7::new_property(NULL | S7::class_character),
    style = S7::new_property(NULL | cell_style),

    # TRUE when this cell is covered by another cell's merge span and should
    # not receive any content requests (insertText, updateTextStyle, etc.).
    consumed = S7::new_property(S7::class_logical, default = FALSE)
  )
)

S7::method(print, table_cell) <- function(x, ...) {
  cli::cli_text(
    "{.strong table_cell} [{x@row_index}, {x@col_index}]: {.val {x@text}}"
  )
  invisible(x)
}
