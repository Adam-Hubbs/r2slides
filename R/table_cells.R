h_align_values <- c("HORIZONTAL_ALIGNMENT_UNSPECIFIED", "LEFT", "CENTER", "RIGHT")

v_align_values <- c("VERTICAL_ALIGNMENT_UNSPECIFIED", "TOP", "MIDDLE", "BOTTOM")


#' Cell styling properties for table cells
#'
#' @param bg_color Background color. Accepts a hex string (e.g. `"#FF0000"`),
#'   a named R color (e.g. `"red"`), an RGB numeric vector `c(r, g, b)` with
#'   values in \[0, 1\], or a theme color string (e.g. `"ACCENT1"`).
#' @param text_style A `text_style` object controlling the appearance of text
#'   within the cell.
#' @param h_align A string specifying horizontal alignment. One of
#'   `"HORIZONTAL_ALIGNMENT_UNSPECIFIED"`, `"LEFT"`, `"CENTER"`, or `"RIGHT"`.
#' @param v_align A string specifying vertical alignment. One of
#'   `"VERTICAL_ALIGNMENT_UNSPECIFIED"`, `"TOP"`, `"MIDDLE"`, or `"BOTTOM"`.
#' @param col_span An integer specifying the number of columns the cell spans.
#' @param row_span An integer specifying the number of rows the cell spans.
#'
#' @export
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
    h_align = S7::new_property(
      NULL | S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("h_align must be a single value")
          }
          if (!(value %in% h_align_values)) {
            return(
              paste0(
                "h_align must be one of: ",
                paste(h_align_values, collapse = ", ")
              )
            )
          }
        }
      }
    ),
    v_align = S7::new_property(
      NULL | S7::class_character,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("v_align must be a single value")
          }
          if (!(value %in% v_align_values)) {
            return(
              paste0(
                "v_align must be one of: ",
                paste(v_align_values, collapse = ", ")
              )
            )
          }
        }
      }
    ),
    col_span = S7::new_property(
      NULL | S7::class_integer,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("col_span must be a single value")
          }
          if (value < 1) {
            return("col_span must be a positive integer")
          }
        }
      }
    ),
    row_span = S7::new_property(
      NULL | S7::class_integer,
      validator = function(value) {
        if (!is.null(value)) {
          if (length(value) != 1) {
            return("row_span must be a single value")
          }
          if (value < 1) {
            return("row_span must be a positive integer")
          }
        }
      }
    )
  )
)

#' A single cell in an r2slides_table
#'
#' @param row_index 0-based integer row index.
#' @param col_index 0-based integer column index.
#' @param text Character string with the cell's text content, or `NULL`.
#' @param style A [cell_style] object, or `NULL`.
#'
#' @export
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
    style = S7::new_property(NULL | cell_style)
  )
)

S7::method(print, table_cell) <- function(x, ...) {
  cli::cli_text(
    "{.strong table_cell} [{x@row_index}, {x@col_index}]: {.val {x@text}}"
  )
  invisible(x)
}
