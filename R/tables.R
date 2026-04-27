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

#' @noRd
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
        if (length(value) != 1) return("n_rows must be a single value")
        if (value < 1L)         return("n_rows must be a positive integer")
      }
    ),

    n_cols = S7::new_property(
      S7::class_integer,
      validator = function(value) {
        if (length(value) != 1) return("n_cols must be a single value")
        if (value < 1L)         return("n_cols must be a positive integer")
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
        if (length(value) != 1) return("header_rows must be a single value")
        if (value < 0L)         return("header_rows must be a non-negative integer")
      }
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
  par_s <- section$styles$pars

  bold          <- ft_read_style(txt_s$bold,          row_i, col_key)
  italic        <- ft_read_style(txt_s$italic,        row_i, col_key)
  underline     <- ft_read_style(txt_s$underlined,    row_i, col_key)
  strikethrough <- ft_read_style(txt_s$strike,        row_i, col_key)
  font_family   <- ft_read_style(txt_s$font.family,   row_i, col_key)
  font_size     <- ft_read_style(txt_s$font.size,     row_i, col_key)

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

  alignment <- switch(
    ft_read_style(par_s$text.align, row_i, col_key) %||% "",
    "left"    = "START",
    "center"  = "CENTER",
    "right"   = "END",
    "justify" = "JUSTIFIED",
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
    baseline_offset = baseline_offset,
    alignment     = alignment
  )
}

# Build a `cell_style` from a flextable section's cell style slots.
#' @noRd
ft_make_cell_style <- function(section, row_i, col_key, ts) {
  cell_s <- section$styles$cells

  raw_bg <- ft_read_style(cell_s$background.color, row_i, col_key)
  bg_color <- if (
    !is.null(raw_bg) && !is.na(raw_bg) &&
    raw_bg != "" && raw_bg != "transparent"
  ) normalize_color(raw_bg) else NULL

  v_align <- switch(
    ft_read_style(cell_s$vertical.align, row_i, col_key) %||% "",
    "top"    = "TOP",
   
    "bottom" = "BOTTOM",
    NULL
  )

  cell_style(
    bg_color   = bg_color,
    text_style = ts,
    v_align    = v_align
  )
}

# Extract span info from one flextable section.
# Returns a list of span specs for origin cells only.
# Each element: list(row_index, col_index, row_span, col_span).
#
# Flextable span matrix naming is counterintuitive:
#   spans$rows[r, c]    = number of COLUMNS the cell spans (horizontal span)
#   spans$columns[r, c] = number of ROWS    the cell spans (vertical span)
# Origin cells have the span count (>= 1); consumed cells have 0.
# We emit only origin cells where at least one span > 1.
#' @noRd
ft_extract_spans <- function(section, col_keys, row_offset) {
  col_span_mat <- section$spans$rows     # value = number of columns spanned
  row_span_mat <- section$spans$columns  # value = number of rows spanned

  if (is.null(col_span_mat) || is.null(row_span_mat)) return(NULL)

  n_rows <- nrow(col_span_mat)
  n_cols <- ncol(col_span_mat)

  results <- vector("list", n_rows * n_cols)
  idx <- 1L

  for (r in seq_len(n_rows)) {
    for (ci in seq_len(n_cols)) {
      cs <- col_span_mat[r, ci]  # columns spanned
      rs <- row_span_mat[r, ci]  # rows spanned

      # Skip consumed cells (value == 0) and no-op single cells (both == 1)
      if (cs < 1L || rs < 1L) next
      if (cs == 1L && rs == 1L) next

      results[[idx]] <- list(
        row_index = as.integer(row_offset + r - 1L),
        col_index = as.integer(ci - 1L),
        row_span  = as.integer(rs),
        col_span  = as.integer(cs)
      )
      idx <- idx + 1L
    }
  }

  results[seq_len(idx - 1L)]
}

# Extract border info from one flextable section.
# Returns a list of border side specs, one per (cell, side) combination.
# Each element: list(row_index, col_index, side, color, width, dash_style).
#
# Flextable border slot naming: "border.color.{side}", "border.width.{side}",
# "border.style.{side}" where side is bottom/top/left/right.
#' @noRd
ft_extract_section_borders <- function(section, col_keys, row_offset) {
  cell_s <- section$styles$cells
  n_rows <- nrow(section$dataset)
  sides  <- c("bottom", "top", "left", "right")

  results <- list()

  for (r in seq_len(n_rows)) {
    for (ci in seq_along(col_keys)) {
      col_key <- col_keys[[ci]]

      for (side in sides) {
        color_slot <- cell_s[[paste0("border.color.", side)]]
        width_slot <- cell_s[[paste0("border.width.", side)]]
        style_slot <- cell_s[[paste0("border.style.", side)]]

        if (is.null(color_slot)) next

        raw_color <- ft_read_style(color_slot, r, col_key)
        raw_width <- if (!is.null(width_slot)) ft_read_style(width_slot, r, col_key) else NULL
        raw_style <- if (!is.null(style_slot)) ft_read_style(style_slot, r, col_key) else NULL

        color <- if (
          !is.null(raw_color) && !is.na(raw_color) &&
          raw_color != "" && raw_color != "transparent"
        ) normalize_color(raw_color) else NULL

        # Width of 0 means "no visible border" in flextable. The Google Slides
        # API rejects weight <= 0, so substitute width=1 with white (#FFFFFF),
        # which is visually invisible and overrides the Google Slides default border.
        no_border <- !is.null(raw_width) && !is.na(raw_width) && raw_width == 0

        if (no_border) {
          results <- c(results, list(list(
            row_index  = as.integer(row_offset + r - 1L),
            col_index  = as.integer(ci - 1L),
            side       = side,
            color      = "#FFFFFF",
            width      = 1,
            dash_style = "SOLID"
          )))
          next
        }

        width <- if (!is.null(raw_width) && !is.na(raw_width) && raw_width > 0) {
          as.double(raw_width)
        } else NULL

        dash_style <- if (!is.null(raw_style) && !is.na(raw_style) && nzchar(raw_style)) {
          unname(ft_dash_style_map[raw_style])
        } else NULL

        if (is.null(color) && is.null(width) && is.null(dash_style)) next

        results <- c(results, list(list(
          row_index  = as.integer(row_offset + r - 1L),
          col_index  = as.integer(ci - 1L),
          side       = side,
          color      = color,
          width      = width,
          dash_style = dash_style
        )))
      }
    }
  }

  results
}

# Apply span information to a list of table_cell objects in-place.
# span_specs: list output of ft_extract_spans (both sections combined).
#' @noRd
apply_spans_to_cells <- function(cells, span_specs) {
  if (length(span_specs) == 0L) return(cells)

  # Build a lookup: "row,col" -> span spec
  lookup <- stats::setNames(
    span_specs,
    vapply(span_specs, \(s) paste0(s$row_index, ",", s$col_index), character(1))
  )

  for (i in seq_along(cells)) {
    cell <- cells[[i]]
    key  <- paste0(cell@row_index, ",", cell@col_index)
    spec <- lookup[[key]]

    if (!is.null(spec) && !is.null(cell@style)) {
      cell@style@col_span <- spec$col_span
      cell@style@row_span <- spec$row_span
      cells[[i]] <- cell
    }
  }

  cells
}

# Apply border information to a list of table_cell objects in-place.
# border_specs: list output of ft_extract_section_borders (both sections combined).
#' @noRd
apply_borders_to_cells <- function(cells, border_specs) {
  if (length(border_specs) == 0L) return(cells)

  # Build lookup: "row,col" -> list of side specs
  lookup <- list()
  for (spec in border_specs) {
    key <- paste0(spec$row_index, ",", spec$col_index)
    lookup[[key]] <- c(lookup[[key]], list(spec))
  }

  for (i in seq_along(cells)) {
    cell  <- cells[[i]]
    key   <- paste0(cell@row_index, ",", cell@col_index)
    specs <- lookup[[key]]

    if (is.null(specs) || is.null(cell@style)) next

    cs <- cell@style
    for (spec in specs) {
      border_val <- make_border_side(spec$color, spec$width, spec$dash_style)
      if (spec$side == "top")    cs@border_top    <- border_val
      if (spec$side == "bottom") cs@border_bottom <- border_val
      if (spec$side == "left")   cs@border_left   <- border_val
      if (spec$side == "right")  cs@border_right  <- border_val
    }
    cell@style    <- cs
    cells[[i]]    <- cell
  }

  cells
}

# Build one updateTableBorderProperties request body.
#' @noRd
build_border_request <- function(table_id, row_index, col_index, position, color, width, dash_style) {
  props  <- list()
  fields <- character()

  if (!is.null(color)) {
    # color_to_solid_fill returns list(solidFill = list(color = ...))
    # tableBorderFill expects the same shape
    props$tableBorderFill <- color_to_solid_fill(color)
    fields <- c(fields, "tableBorderFill")
  }

  if (!is.null(width)) {
    props$weight <- list(magnitude = width, unit = "PT")
    fields <- c(fields, "weight")
  }

  if (!is.null(dash_style) && !is.na(dash_style)) {
    props$dashStyle <- dash_style
    fields <- c(fields, "dashStyle")
  }

  if (length(props) == 0L) return(NULL)

  list(
    updateTableBorderProperties = list(
      objectId      = table_id,
      tableRange    = list(
        location   = list(rowIndex = row_index, columnIndex = col_index),
        rowSpan    = 1L,
        columnSpan = 1L
      ),
      borderPosition        = position,
      tableBorderProperties = props,
      fields                = paste(fields, collapse = ",")
    )
  )
}

# Build one mergeTableCells request body.
#' @noRd
build_merge_request <- function(table_id, row_index, col_index, row_span, col_span) {
  list(
    mergeTableCells = list(
      objectId   = table_id,
      tableRange = list(
        location   = list(rowIndex = row_index, columnIndex = col_index),
        rowSpan    = row_span,
        columnSpan = col_span
      )
    )
  )
}

# Map a cell border side name to the Google Slides border position string.
#' @noRd
side_to_border_position <- function(side) {
  switch(side,
    top    = "TOP",
    bottom = "BOTTOM",
    left   = "LEFT",
    right  = "RIGHT"
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

  col_keys      <- x$col_keys
  n_cols        <- length(col_keys)
  n_header_rows <- nrow(x$header$dataset)
  n_body_rows   <- nrow(x$body$dataset)
  n_rows        <- n_header_rows + n_body_rows

  col_widths  <- unname(x$body$colwidths) * 72
  row_heights <- c(x$header$rowheights, x$body$rowheights) * 72

  extract_section_cells <- function(section, row_offset) {
    n_sec_rows <- nrow(section$dataset)
    cells <- vector("list", n_sec_rows * n_cols)
    idx <- 1L

    # Span matrices: 0 means the cell is consumed by a neighbouring merge.
    #   spans$rows[r, ci]    = col-span count (0 = consumed horizontally)
    #   spans$columns[r, ci] = row-span count (0 = consumed vertically)
    col_span_mat <- section$spans$rows
    row_span_mat <- section$spans$columns

    for (r in seq_len(n_sec_rows)) {
      for (ci in seq_along(col_keys)) {
        col_key <- col_keys[[ci]]

        # A cell is consumed when either span dimension is 0.
        is_consumed <- !is.null(col_span_mat) &&
          (col_span_mat[r, ci] == 0L || row_span_mat[r, ci] == 0L)

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
          text      = txt,
          style     = cs,
          consumed  = isTRUE(is_consumed)
        )
        idx <- idx + 1L
      }
    }
    cells
  }

  header_cells <- extract_section_cells(x$header, row_offset = 0L)
  body_cells   <- extract_section_cells(x$body,   row_offset = n_header_rows)
  all_cells    <- c(header_cells, body_cells)

  # Spans
  header_spans <- ft_extract_spans(x$header, col_keys, row_offset = 0L)
  body_spans   <- ft_extract_spans(x$body,   col_keys, row_offset = n_header_rows)
  all_cells    <- apply_spans_to_cells(all_cells, c(header_spans, body_spans))

  # Borders
  header_borders <- ft_extract_section_borders(x$header, col_keys, row_offset = 0L)
  body_borders   <- ft_extract_section_borders(x$body,   col_keys, row_offset = n_header_rows)
  all_cells      <- apply_borders_to_cells(all_cells, c(header_borders, body_borders))

  r2slides_table(
    cells       = all_cells,
    n_rows      = as.integer(n_rows),
    n_cols      = as.integer(n_cols),
    col_widths  = col_widths,
    row_heights = row_heights,
    header_rows = as.integer(n_header_rows)
  )
}

#' Build Google Slides API requests for an r2slides_table
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
              width  = list(magnitude = position@width_emu,  unit = "EMU"),
              height = list(magnitude = position@height_emu, unit = "EMU")
            ),
            transform = list(
              scaleX     = position@scaleX,
              scaleY     = position@scaleY,
              shearX     = position@shearX,
              shearY     = position@shearY,
              translateX = position@left_emu,
              translateY = position@top_emu,
              unit       = "EMU"
            )
          ),
          rows    = table@n_rows,
          columns = table@n_cols
        )
      )
    )
  )

  # ── 2. updateTableColumnProperties ──────────────────────────────────────────
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

  col_width_req <- if (!is.null(col_width_reqs)) list(requests = col_width_reqs) else NULL

  # ── 3. Per-cell requests ─────────────────────────────────────────────────────
  cell_reqs  <- list()
  merge_reqs <- list()

  for (cell in table@cells) {
    ri <- cell@row_index
    ci <- cell@col_index
    cell_location <- list(rowIndex = ri, columnIndex = ci)

    # Consumed cells are covered by a merge — skip all content requests.
    # (Flextable duplicates text into consumed cells; we must not write it.)
    # Merge requests are only ever on origin cells, so nothing is lost here.
    if (isTRUE(cell@consumed)) next

    has_text <- !is.null(cell@text) && nzchar(cell@text)

    if (has_text) {
      cell_reqs <- c(cell_reqs, list(list(
        insertText = list(
          objectId       = table_id,
          cellLocation   = cell_location,
          text           = cell@text,
          insertionIndex = 0L
        )
      )))
    }

    if (has_text && !is.null(cell@style) && !is.null(cell@style@text_style)) {
      ts <- cell@style@text_style

      if (S7::S7_inherits(ts, text_style)) {
        if (length(ts@fields) > 0) {
          cell_reqs <- c(cell_reqs, list(list(
            updateTextStyle = list(
              objectId     = table_id,
              cellLocation = cell_location,
              textRange    = list(type = "ALL"),
              style        = ts@style,
              fields       = paste(ts@fields, collapse = ",")
            )
          )))
        }

        if (length(ts@paragraph_fields) > 0) {
          cell_reqs <- c(cell_reqs, list(list(
            updateParagraphStyle = list(
              objectId     = table_id,
              cellLocation = cell_location,
              textRange    = list(type = "ALL"),
              style        = ts@paragraph_style,
              fields       = paste(ts@paragraph_fields, collapse = ",")
            )
          )))
        }
      }
    }

    if (has_text && !is.null(cell@style)) {
      cs <- cell@style
      tcp <- list()
      tcp_fields <- character()

      if (!is.null(cs@bg_color)) {
        tcp$tableCellBackgroundFill <- color_to_solid_fill(cs@bg_color)
        tcp_fields <- c(tcp_fields, "tableCellBackgroundFill")
      }

      if (!is.null(cs@v_align)) {
        content_alignment <- switch(
          cs@v_align,
          "TOP"    = "TOP",
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
        row_span <- cs@row_span %||% 1L
        col_span <- cs@col_span %||% 1L

        cell_reqs <- c(cell_reqs, list(list(
          updateTableCellProperties = list(
            objectId   = table_id,
            tableRange = list(
              location   = cell_location,
              rowSpan    = row_span,
              columnSpan = col_span
            ),
            tableCellProperties = tcp,
            fields = paste(tcp_fields, collapse = ",")
          )
        )))
      }
    }

    # Collect merge requests for cells with spans > 1
    if (!is.null(cell@style)) {
      rs <- cell@style@row_span %||% 1L
      cs_span <- cell@style@col_span %||% 1L
      if (rs > 1L || cs_span > 1L) {
        merge_reqs <- c(merge_reqs, list(
          build_merge_request(table_id, ri, ci, rs, cs_span)
        ))
      }
    }
  }

  cell_req  <- list(requests = cell_reqs)
  merge_req <- if (length(merge_reqs) > 0L) list(requests = merge_reqs) else NULL

  # ── 4. Border requests ───────────────────────────────────────────────────────
  # Each interior border is a single physical line shared between two adjacent
  # cells. Sending both a BOTTOM from the cell above AND a TOP from the cell
  # below for the same physical line means the second request silently
  # overwrites the first (last write wins in batchUpdate).
  #
  # Strategy: canonicalise every border to the "owner" cell before emitting:
  #   - TOP at ri > 0  → emit as BOTTOM at (ri - 1, ci)   [cell above owns it]
  #   - LEFT at ci > 0 → emit as RIGHT at (ri, ci - 1)    [cell to left owns it]
  #   - BOTTOM / RIGHT are always emitted from the current cell as-is.
  #   - Outer-edge TOP (ri == 0) and LEFT (ci == 0) emit directly as TOP/LEFT.
  #
  # We collect into a named map keyed by "ri,ci,POSITION" so that if both the
  # cell above and the cell below store a value for the same shared line, the
  # last one written wins — which is fine because flextable normalises adjacent
  # cells to agree on shared borders.
  border_map <- list()

  for (cell in table@cells) {
    if (is.null(cell@style)) next
    ri <- cell@row_index   # 0-based
    ci <- cell@col_index   # 0-based
    cs <- cell@style

    sides <- list(
      top    = list(border = cs@border_top,    emit_ri = ri - 1L, emit_ci = ci,      pos = "BOTTOM"),
      bottom = list(border = cs@border_bottom, emit_ri = ri,      emit_ci = ci,      pos = "BOTTOM"),
      left   = list(border = cs@border_left,   emit_ri = ri,      emit_ci = ci - 1L, pos = "RIGHT"),
      right  = list(border = cs@border_right,  emit_ri = ri,      emit_ci = ci,      pos = "RIGHT")
    )

    # Outer edges: TOP at ri==0 emits as TOP, LEFT at ci==0 emits as LEFT
    if (ri == 0L) sides$top  <- list(border = cs@border_top,  emit_ri = ri, emit_ci = ci, pos = "TOP")
    if (ci == 0L) sides$left <- list(border = cs@border_left, emit_ri = ri, emit_ci = ci, pos = "LEFT")

    for (s in sides) {
      if (is.null(s$border)) next
      key <- paste(s$emit_ri, s$emit_ci, s$pos, sep = ",")
      border_map[[key]] <- list(
        row_index  = s$emit_ri,
        col_index  = s$emit_ci,
        position   = s$pos,
        color      = s$border$color,
        width      = s$border$width,
        dash_style = s$border$dash_style
      )
    }
  }

  border_reqs <- Filter(Negate(is.null), lapply(border_map, \(b) {
    build_border_request(
      table_id   = table_id,
      row_index  = b$row_index,
      col_index  = b$col_index,
      position   = b$position,
      color      = b$color,
      width      = b$width,
      dash_style = b$dash_style
    )
  }))

  border_req <- if (length(border_reqs) > 0L) list(requests = border_reqs) else NULL

  # ── 5. updateTableRowProperties ─────────────────────────────────────────────
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
    create      = create_req,
    col_widths  = col_width_req,
    cells       = cell_req,
    merges      = merge_req,
    borders     = border_req,
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
  order    = c("front", "back"),
  table_id = NULL,
  debug    = FALSE,
  token    = NULL
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

  if (!S7::S7_inherits(table, r2slides_table)) {
    table <- as_r2slides_table(table)
  }

  params <- list(presentationId = slide_obj@presentation$presentation_id)

  reqs <- create_table_requests(
    table    = table,
    slide_id = slide_obj@slide_id,
    position = position,
    table_id = table_id
  )

  table_id <- reqs$create$requests[[1]]$createTable$objectId

  if (debug) return(reqs)

  do_batch <- function(req) {
    if (!is.null(req) && length(req$requests) > 0L) {
      query(
        endpoint = "slides.presentations.batchUpdate",
        params   = params,
        body     = req,
        base     = "slides",
        token    = token
      )
    }
  }

  # Order matters: create -> col widths -> cells -> merges -> borders -> row heights
  do_batch(reqs$create)
  do_batch(reqs$col_widths)
  do_batch(reqs$cells)
  do_batch(reqs$merges)
  do_batch(reqs$borders)
  do_batch(reqs$row_heights)

  if (order == "back") {
    zorder_by_id(
      presentation_id = slide_obj@presentation$presentation_id,
      element_id      = table_id,
      operation       = resolve_zorder_op(order)
    )
  }

  invisible(slide_obj)
}
