border_position_values <- c(
  "ALL",
  "BOTTOM",
  "INNER",
  "INNER_HORIZONTAL",
  "INNER_VERTICAL",
  "LEFT",
  "OUTER",
  "RIGHT",
  "TOP"
)

border_style_values <- c(
  "DASH",
  "DASH_DOT",
  "LONG_DASH",
  "LONG_DASH_DOT",
  "SOLID",
  "SOLID_HEAVY",
  "SOLID_MEDIUM",
  "TRANSPARENT"
)

#' Intermediate table representation for Google Slides
#'
#' `r2slides_table` is a backend-agnostic intermediate representation of a
#' table, intended to be the target output of [as_r2slides_table()] methods for
#' table objects such as those produced by **gt** or **flextable**. The object
#' captures cell locations and styles, dimensions, and border styling so that
#' it can be rendered as a Google Slides table element.
#'
#' @param cells A list of [table_cell] objects defining the 0-based row/column
#'   start index of each cell along with its optional [cell_style].
#' @param n_rows A positive integer giving the total number of rows.
#' @param n_cols A positive integer giving the total number of columns.
#' @param col_widths A numeric vector of column widths in points. Must have
#'   length equal to `n_cols` when provided.
#' @param row_heights A numeric vector of row heights in points. Must have
#'   length equal to `n_rows` when provided.
#' @param header_rows A non-negative integer giving the number of header rows.
#'   Defaults to `0L`.
#'
#' @export
r2slides_table <- S7::new_class(
  "r2slides_table",
  properties = list(
    cells = S7::new_property(
      S7::class_list,
      validator = function(value) {
        ok <- vapply(value, \(x) S7::S7_inherits(x, table_cell), logical(1))
        if (!all(ok)) return("cells must be a list of table_cell objects")
      }
    ),
    n_rows = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          return("n_rows must be a single value")
        }
        if (value < 1L) return("n_rows must be a positive integer")
      }
    ),
    n_cols = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) {
          return("n_cols must be a single value")
        }
        if (value < 1L) return("n_cols must be a positive integer")
      }
    ),
    col_widths = S7::new_property(
      NULL | S7::class_numeric,
      validator = function(value) {
        if (!is.null(value) && any(value <= 0)) {
          return("all col_widths must be positive")
        }
      }
    ),
    row_heights = S7::new_property(
      NULL | S7::class_numeric,
      validator = function(value) {
        if (!is.null(value) && any(value <= 0)) {
          return("all row_heights must be positive")
        }
      }
    ),
    header_rows = S7::new_property(
      S7::class_integer,
      default = 0L,
      validator = function(value) {
        if (length(value) != 1) {
          return("header_rows must be a single value")
        }
        if (value < 0L) return("header_rows must be a non-negative integer")
      }
    ),
    border_styles = S7::new_property(
      S7::class_any,
      default = NULL
    )
  ),
  validator = function(self) {
    if (self@header_rows > self@n_rows) {
      return("header_rows cannot exceed n_rows")
    }
  }
)


S7::method(print, r2slides_table) <- function(x, ...) {
  cli::cli_text("{.strong r2slides_table}")
  cli::cli_text("Dimensions : {.val {x@n_rows}} rows x {.val {x@n_cols}} cols")
  cli::cli_text("Header rows: {.val {x@header_rows}}")
  cli::cli_text("Cells      : {.val {length(x@cells)}}")
}

# Read one value from a flextable fpstruct-style slot (`$data` matrix +
# `$default` scalar), falling back to the default when the cell value is
# `NULL` or `NA`.
#' @noRd
ft_read_style <- function(slot, row_i, col_key) {
  val <- slot$data[row_i, col_key]
  if (is.null(val) || (length(val) == 1 && is.na(val))) slot$default else val
}

# Build a `text_style` from a flextable section's text-style slots for one cell.
#' @noRd
ft_make_text_style <- function(section, row_i, col_key) {
  txt_s <- section$styles$text

  bold         <- ft_read_style(txt_s$bold,          row_i, col_key)
  italic       <- ft_read_style(txt_s$italic,        row_i, col_key)
  underline    <- ft_read_style(txt_s$underlined,    row_i, col_key)
  strikethrough <- ft_read_style(txt_s$strike,       row_i, col_key)
  font_family  <- ft_read_style(txt_s$font.family,   row_i, col_key)
  font_size    <- ft_read_style(txt_s$font.size,     row_i, col_key)

  # normalize_color handles hex, named R colors, and transparent/NA -> NULL
  raw_text_color <- ft_read_style(txt_s$color,         row_i, col_key)
  raw_bg_color   <- ft_read_style(txt_s$shading.color, row_i, col_key)

  text_color <- if (
    !is.null(raw_text_color) && !is.na(raw_text_color) &&
    raw_text_color != "" && raw_text_color != "transparent"
  ) normalize_color(raw_text_color) else NULL

  bg_color <- if (
    !is.null(raw_bg_color) && !is.na(raw_bg_color) &&
    raw_bg_color != "" && raw_bg_color != "transparent"
  ) normalize_color(raw_bg_color) else NULL

  vert_align <- ft_read_style(txt_s$vertical.align, row_i, col_key)
  baseline_offset <- switch(
    vert_align %||% "",
    "superscript" = "SUPERSCRIPT",
    "subscript"   = "SUBSCRIPT",
    NULL
  )

  text_style(
    bold          = if (isTRUE(bold) || identical(bold, FALSE)) bold else NULL,
    italic        = if (isTRUE(italic) || identical(italic, FALSE)) italic else NULL,
    underline     = if (isTRUE(underline) || identical(underline, FALSE)) underline else NULL,
    strikethrough = if (isTRUE(strikethrough) || identical(strikethrough, FALSE)) strikethrough else NULL,
    font_family   = if (!is.null(font_family) && nzchar(font_family)) font_family else NULL,
    font_size     = if (!is.null(font_size) && is.numeric(font_size) && font_size > 0) as.double(font_size) else NULL,
    text_color    = text_color,
    bg_color      = bg_color,
    baseline_offset = baseline_offset
  )
}

# Build a `cell_style` from a flextable section's cell/paragraph style slots.
#' @noRd
ft_make_cell_style <- function(section, row_i, col_key, ts) {
  cell_s <- section$styles$cells
  par_s  <- section$styles$pars

  raw_bg <- ft_read_style(cell_s$background.color, row_i, col_key)
  bg_color <- if (
    !is.null(raw_bg) && !is.na(raw_bg) &&
    raw_bg != "" && raw_bg != "transparent"
  ) normalize_color(raw_bg) else NULL

  h_align <- switch(
    ft_read_style(par_s$text.align, row_i, col_key) %||% "",
    "left"   = "LEFT",
    "center" = "CENTER",
    "right"  = "RIGHT",
    NULL
  )

  v_align <- switch(
    ft_read_style(cell_s$vertical.align, row_i, col_key) %||% "",
    "top"    = "TOP",
    "center" = "MIDDLE",
    "bottom" = "BOTTOM",
    NULL
  )

  cell_style(
    bg_color   = bg_color,
    text_style = ts,
    h_align    = h_align,
    v_align    = v_align
  )
}


#' Convert a table object to an r2slides_table
#'
#' @description
#' `as_r2slides_table()` is a generic function that converts a table object
#' (e.g. from **gt** or **flextable**) into an [r2slides_table] object suitable
#' for rendering as a Google Slides table element.
#'
#' Methods for specific table classes should return an [r2slides_table] object.
#'
#' @param x A table object. Methods are expected for classes such as `gt_tbl`
#'   (from **gt**) and `flextable` (from **flextable**).
#' @param ... Additional arguments passed to methods.
#'
#' @return An [r2slides_table] object.
#'
#' @export
as_r2slides_table <- S7::new_generic("as_r2slides_table", "x")

method(as_r2slides_table, S7::new_S3_class("flextable")) <- function(x) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg flextable} is required but is not installed.")
  }

  col_keys <- x$col_keys
  n_cols <- length(col_keys)
  n_header_rows <- nrow(x$header$dataset)
  n_body_rows <- nrow(x$body$dataset)
  n_rows <- n_header_rows + n_body_rows

  # Widths/heights: flextable stores in inches; convert to points (* 72)
  # colwidths is a positional unnamed vector matching col_keys order
  col_widths <- unname(x$body$colwidths) * 72
  row_heights <- c(x$header$rowheights, x$body$rowheights) * 72

  # Extract all cells from one section (header or body).
  # row_offset: 0-based index offset (0 for header, n_header_rows for body).
  extract_section_cells <- function(section, row_offset) {
    n_sec_rows <- nrow(section$dataset)
    cells <- vector("list", n_sec_rows * n_cols)
    idx <- 1L

    for (r in seq_len(n_sec_rows)) {
      for (ci in seq_along(col_keys)) {
        col_key <- col_keys[[ci]]

        ts <- ft_make_text_style(section, r, col_key)
        cs <- ft_make_cell_style(section, r, col_key, ts)

        chunk_df <- section$content$data[r, col_key][[1]]
        txt <- if (is.null(chunk_df) || !is.data.frame(chunk_df)) {
          NULL
        } else {
          paste(chunk_df$txt, collapse = "")
        }

        cells[[idx]] <- table_cell(
          row_index = as.integer(row_offset + r - 1L),
          col_index = as.integer(ci - 1L),
          text = txt,
          style = cs
        )
        idx <- idx + 1L
      }
    }
    cells
  }

  header_cells <- extract_section_cells(x$header, row_offset = 0L)
  body_cells <- extract_section_cells(x$body, row_offset = n_header_rows)

  r2slides_table(
    cells = c(header_cells, body_cells),
    n_rows = as.integer(n_rows),
    n_cols = as.integer(n_cols),
    col_widths = col_widths,
    row_heights = row_heights,
    header_rows = as.integer(n_header_rows)
  )
}

#' Build Google Slides API requests for an r2slides_table
#'
#' Returns a named list of batchUpdate request bodies that, when submitted in
#' order, create and populate a table on the given slide.  The list has four
#' elements:
#'
#' \describe{
#'   \item{`create`}{A single `createTable` request.}
#'   \item{`col_widths`}{One `updateTableColumnProperties` request per column.}
#'   \item{`cells`}{`insertText`, `updateTextStyle`, and
#'     `updateTableCellProperties` requests for every cell that has content or
#'     styling.}
#'   \item{`row_heights`}{One `updateTableRowProperties` request per row.}
#' }
#'
#' @param table An [r2slides_table] object.
#' @param slide_id Character string. The Google Slides page object ID of the
#'   target slide.
#' @param position A [slide_position] object describing where to place the
#'   table on the slide.
#' @param table_id Optional character string. The object ID to assign to the
#'   new table.  If `NULL` a unique ID is generated automatically.
#'
#' @return A named list with elements `create`, `col_widths`, `cells`, and
#'   `row_heights`, each being a list with a `requests` element ready for
#'   [query()].
#'
#' @noRd
create_table_requests <- function(table, slide_id, position, table_id = NULL) {
  if (is.null(table_id)) {
    table_id <- paste0(
      "table_",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "_",
      sample(1000L:9999L, 1L)
    )
  }

  # ── 1. createTable ──────────────────────────────────────────────────────────
  create_req <- list(
    requests = list(
      list(
        createTable = list(
          objectId = table_id,
          elementProperties = list(
            pageObjectId = slide_id,
            size = list(
              width = list(magnitude = position@width_emu, unit = "EMU"),
              height = list(magnitude = position@height_emu, unit = "EMU")
            ),
            transform = list(
              scaleX = position@scaleX,
              scaleY = position@scaleY,
              shearX = position@shearX,
              shearY = position@shearY,
              translateX = position@left_emu,
              translateY = position@top_emu,
              unit = "EMU"
            )
          ),
          rows = table@n_rows,
          columns = table@n_cols
        )
      )
    )
  )

  # ── 2. updateTableColumnProperties (column widths) ──────────────────────────
  col_width_reqs <- if (!is.null(table@col_widths)) {
    purrr::imap(table@col_widths, \(w, i) {
      list(
        updateTableColumnProperties = list(
          objectId = table_id,
          columnIndices = list(i - 1L),
          tableColumnProperties = list(
            columnWidth = list(magnitude = w, unit = "PT")
          ),
          fields = "columnWidth"
        )
      )
    })
  } else {
    NULL
  }

  if (is.null(col_width_reqs)) {
    col_width_req <- NULL
  } else {
    col_width_req <- list(requests = col_width_reqs)
  }

  # ── 3. Per-cell: insertText + updateTextStyle + updateTableCellProperties ───
  cell_reqs <- list()

  for (cell in table@cells) {
    ri <- cell@row_index # 0-based
    ci <- cell@col_index

    cell_location <- list(
      rowIndex = ri,
      columnIndex = ci
    )

    has_text <- !is.null(cell@text) && nzchar(cell@text)

    # insertText
    if (has_text) {
      cell_reqs <- c(
        cell_reqs,
        list(
          list(
            insertText = list(
              objectId = table_id,
              cellLocation = cell_location,
              text = cell@text,
              insertionIndex = 0L
            )
          )
        )
      )
    }

    # updateTextStyle and updateTableCellProperties only when the cell has text
    if (has_text) {
      # updateTextStyle (only when there are fields to set)
      if (!is.null(cell@style) && !is.null(cell@style@text_style)) {
        ts <- cell@style@text_style
        if (S7::S7_inherits(ts, text_style) && length(ts@fields) > 0) {
          cell_reqs <- c(
            cell_reqs,
            list(
              list(
                updateTextStyle = list(
                  objectId = table_id,
                  cellLocation = cell_location,
                  textRange = list(type = "ALL"),
                  style = ts@style,
                  fields = paste(ts@fields, collapse = ",")
                )
              )
            )
          )
        }
      }

      # updateTableCellProperties (bg color + content alignment)
      if (!is.null(cell@style)) {
        cs <- cell@style
        tcp <- list()
        tcp_fields <- character()

        if (!is.null(cs@bg_color)) {
          tcp$tableCellBackgroundFill <- color_to_solid_fill(cs@bg_color)
          tcp_fields <- c(tcp_fields, "tableCellBackgroundFill")
        }

        # Google Slides contentAlignment maps from vertical alignment
        if (!is.null(cs@h_align)) {
          content_alignment <- switch(
            cs@v_align,
            "TOP" = "TOP",
            "MIDDLE" = "MIDDLE",
            "BOTTOM" = "BOTTOM",
            NULL
          )
          if (!is.null(content_alignment)) {
            tcp$contentAlignment <- content_alignment
            tcp_fields <- c(tcp_fields, "contentAlignment")
          }
        }

        if (length(tcp) > 0) {
          cell_reqs <- c(
            cell_reqs,
            list(
              list(
                updateTableCellProperties = list(
                  objectId = table_id,
                  tableRange = list(
                    location = cell_location,
                    rowSpan = 1L,
                    columnSpan = 1L
                  ),
                  tableCellProperties = tcp,
                  fields = paste(tcp_fields, collapse = ",")
                )
              )
            )
          )
        }
      }
    }
  }

  cell_req <- list(requests = cell_reqs)

  # ── 4. updateTableRowProperties (row heights) ────────────────────────────────
  row_height_reqs <- if (!is.null(table@row_heights)) {
    purrr::imap(table@row_heights, \(h, i) {
      list(
        updateTableRowProperties = list(
          objectId = table_id,
          rowIndices = list(i - 1L),
          tableRowProperties = list(
            minRowHeight = list(magnitude = h, unit = "PT")
          ),
          fields = "minRowHeight"
        )
      )
    })
  } else {
    list()
  }

  row_height_req <- list(requests = row_height_reqs)

  list(
    create = create_req,
    col_widths = col_width_req,
    cells = cell_req,
    row_heights = row_height_req
  ) |> purrr::compact()
}


#' Add a table to a Google Slides slide
#'
#' Converts `table` to an [r2slides_table] (if it isn't one already), builds
#' the necessary batchUpdate requests, and submits them in order to the Slides
#' API.
#'
#' @param slide_obj A `r2slides::slide` object.
#' @param table An [r2slides_table], or any object for which an
#'   [as_r2slides_table()] method exists (e.g. a **flextable**).
#' @param position A [slide_position] describing where to place the table.
#' @param order One of `"front"` or `"back"`. Controls the Z-order of the
#'   created element. Default: `"front"`.
#' @param table_id Optional character string. Object ID to assign to the new
#'   table. Auto-generated when `NULL`.
#' @param debug Logical. When `TRUE` the requests are returned instead of
#'   submitted to the API. Default: `FALSE`.
#' @param token Optional OAuth2 token. Defaults to `r2slides_token()`.
#'
#' @returns The `slide_obj` (invisibly).
#'
#' @export
add_table <- function(
  slide_obj,
  table,
  position,
  order = c("front", "back"),
  table_id = NULL,
  debug = FALSE,
  token = NULL
) {
  order <- rlang::arg_match(order)

  if (!S7::S7_inherits(slide_obj, slide)) {
    cli::cli_abort(
      "{.var slide_obj} must be an object of class {.cls r2slides::slide}"
    )
  }

  if (!S7::S7_inherits(position, slide_position)) {
    cli::cli_abort(
      "{.var position} must be an object of class {.cls r2slides::slide_position}"
    )
  }

  # Convert to r2slides_table if needed
  if (!S7::S7_inherits(table, r2slides_table)) {
    table <- as_r2slides_table(table)
  }

  params <- list(presentationId = slide_obj@presentation$presentation_id)

  reqs <- create_table_requests(
    table = table,
    slide_id = slide_obj@slide_id,
    position = position,
    table_id = table_id
  )

  # Recover the table_id that was actually used 
  table_id <- reqs$create$requests[[1]]$createTable$objectId

  if (debug) {
    return(reqs)
  }

  # Submit in order: create -> col widths -> cells -> row heights
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = params,
    body = reqs$create,
    base = "slides",
    token = token
  )

  if (length(reqs$col_widths$requests) > 0) {
    query(
      endpoint = "slides.presentations.batchUpdate",
      params = params,
      body = reqs$col_widths,
      base = "slides",
      token = token
    )
  }

  if (length(reqs$cells$requests) > 0) {
    query(
      endpoint = "slides.presentations.batchUpdate",
      params = params,
      body = reqs$cells,
      base = "slides",
      token = token
    )
  }

  if (length(reqs$row_heights$requests) > 0) {
    query(
      endpoint = "slides.presentations.batchUpdate",
      params = params,
      body = reqs$row_heights,
      base = "slides",
      token = token
    )
  }

  # Apply Z-order (only "back" needs an explicit call; "front" is the default)
  if (order == "back") {
    zorder_by_id(
      presentation_id = slide_obj@presentation$presentation_id,
      element_id = table_id,
      operation = resolve_zorder_op(order)
    )
  }

  invisible(slide_obj)
}
