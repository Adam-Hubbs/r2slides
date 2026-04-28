test_that("as_r2slides_table: plain flextable has correct dimensions and cell count", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  # 1 header row + 3 body rows, 3 cols
  expect_s3_class(r2, "r2slides::r2slides_table")
  expect_equal(r2@n_rows, 4L)
  expect_equal(r2@n_cols, 3L)
  expect_equal(r2@header_rows, 1L)
  expect_equal(length(r2@cells), 12L)
})

test_that("as_r2slides_table: cell indices are 0-based and cover the full grid", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  row_indices <- vapply(r2@cells, \(c) c@row_index, integer(1))
  col_indices <- vapply(r2@cells, \(c) c@col_index, integer(1))

  expect_equal(sort(unique(row_indices)), 0L:3L)
  expect_equal(sort(unique(col_indices)), 0L:2L)
})

test_that("as_r2slides_table: header cell text matches column names", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  header_cells <- Filter(\(c) c@row_index == 0L, r2@cells)
  header_text <- vapply(header_cells, \(c) c@text %||% "", character(1))

  expect_equal(sort(header_text), sort(c("a", "b", "c")))
})

test_that("as_r2slides_table: body cell text is correctly extracted", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  cell <- Filter(\(c) c@row_index == 1L && c@col_index == 0L, r2@cells)[[1]]
  expect_equal(cell@text, "r1")
})

test_that("as_r2slides_table: col_widths and row_heights are positive numerics", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  expect_length(r2@col_widths, r2@n_cols)
  expect_length(r2@row_heights, r2@n_rows)
  expect_true(all(r2@col_widths > 0))
  expect_true(all(r2@row_heights > 0))
})

# -- Merges --

test_that("as_r2slides_table: merged cells get correct row_span and col_span", {
  ft <- make_merged_ft()
  r2 <- as_r2slides_table(ft)

  # merge_at(i = 1:2, j = 1) spans 2 body rows (row_index 1 & 2), col 0
  origin <- Filter(\(c) c@row_index == 1L && c@col_index == 0L, r2@cells)[[1]]

  expect_equal(origin@style@row_span, 2L)
  expect_equal(origin@style@col_span, 1L)
})

test_that("as_r2slides_table: non-merged cells have NULL span", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  all_null <- all(vapply(
    r2@cells,
    \(c) {
      is.null(c@style@row_span) && is.null(c@style@col_span)
    },
    logical(1)
  ))

  expect_true(all_null)
})

# -- Borders --

test_that("as_r2slides_table: explicit border colour and width are captured", {
  ft <- make_bordered_ft()
  r2 <- as_r2slides_table(ft)

  # Header row (row_index=0) has an explicit top border (width=3, no alpha)
  cell_hdr <- Filter(\(c) c@row_index == 0L && c@col_index == 0L, r2@cells)[[1]]
  top_color <- cell_hdr@style@border_top$color
  expect_true(S7::S7_inherits(top_color, transparent_color))
  expect_null(top_color@alpha) # opaque: alpha not set
  expect_equal(cell_hdr@style@border_top$width, 3)

  # Bottom border of last body row (row_index=5) is also an explicit visible border
  cell_bot <- Filter(\(c) c@row_index == 5L && c@col_index == 0L, r2@cells)[[1]]
  bot_color <- cell_bot@style@border_bottom$color
  expect_true(S7::S7_inherits(bot_color, transparent_color))
  expect_null(bot_color@alpha)
  expect_equal(cell_bot@style@border_bottom$width, 1.5)
})

test_that("as_r2slides_table: zero-width borders are stored as transparent (alpha=0)", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  # Zero-width flextable borders become width=1, alpha=0 in the r2slides_table.
  # The Slides API rejects weight<=0, so width stays 1; alpha=0 makes it invisible.
  some_cell <- Filter(\(c) c@row_index == 2L && c@col_index == 1L, r2@cells)[[
    1
  ]]
  top_color <- some_cell@style@border_top$color
  expect_equal(some_cell@style@border_top$width, 1)
  expect_equal(some_cell@style@border_top$dash_style, "SOLID")
  expect_true(S7::S7_inherits(top_color, transparent_color))
  expect_equal(top_color@alpha, 0)
})

# -- create_table_requests: structure --

test_that("create_table_requests: returns expected top-level keys", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_1"
  )

  expect_named(
    reqs,
    c("create", "col_widths", "cells", "borders", "row_heights"),
    ignore.order = TRUE
  )
})

test_that("create_table_requests: createTable request has correct dimensions and id", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_plain"
  )

  ct <- reqs$create$requests[[1]]$createTable
  expect_equal(ct$objectId, "tbl_plain")
  expect_equal(ct$rows, 4L)
  expect_equal(ct$columns, 3L)
  expect_equal(ct$elementProperties$pageObjectId, "slide_abc")
})

test_that("create_table_requests: col_widths has one request per column", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_cw"
  )

  expect_length(reqs$col_widths$requests, r2@n_cols)
})

test_that("create_table_requests: row_heights has one request per row", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_rh"
  )

  expect_length(reqs$row_heights$requests, r2@n_rows)
})

test_that("create_table_requests: insertText requests match non-empty cells", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_txt"
  )

  text_reqs <- Filter(\(r) !is.null(r$insertText), reqs$cells$requests)
  inserted <- vapply(text_reqs, \(r) r$insertText$text, character(1))

  expect_true("r1" %in% inserted)
  expect_true("a" %in% inserted)
})

# -- create_table_requests: merges --

test_that("create_table_requests: merge produces a mergeTableCells request", {
  r2 <- as_r2slides_table(make_merged_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_mg"
  )

  expect_false(is.null(reqs$merges))
  expect_length(reqs$merges$requests, 1L)

  merge <- reqs$merges$requests[[1]]$mergeTableCells
  expect_equal(merge$tableRange$location$rowIndex, 1L)
  expect_equal(merge$tableRange$location$columnIndex, 0L)
  expect_equal(merge$tableRange$rowSpan, 2L)
  expect_equal(merge$tableRange$columnSpan, 1L)
})

test_that("create_table_requests: no merges key when table has no merged cells", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_nom"
  )

  expect_null(reqs$merges)
})

# -- create_table_requests: borders --

test_that("create_table_requests: explicit borders produce border requests", {
  r2 <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_bd"
  )

  expect_false(is.null(reqs$borders))
  expect_gt(length(reqs$borders$requests), 0L)
})

test_that("create_table_requests: visible header top border request is correct", {
  r2 <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_rb"
  )

  # Header row (row_index=0) has a visible top border: width=3, emitted as TOP
  # directly from that cell.
  top_reqs <- find_border_req(reqs$borders$requests, 0L, 0L, "TOP")
  expect_length(top_reqs, 1L)

  props <- top_reqs[[1]]$updateTableBorderProperties$tableBorderProperties
  expect_equal(props$weight$magnitude, 3)
  expect_null(props$tableBorderFill$solidFill$alpha) # opaque, no alpha field
})

test_that("create_table_requests: visible bottom border on last row is correct", {
  r2 <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_bb"
  )

  # Last body row (row_index=5, col=0) has a visible bottom border: width=1.5
  bot_reqs <- find_border_req(reqs$borders$requests, 5L, 0L, "BOTTOM")
  expect_length(bot_reqs, 1L)

  props <- bot_reqs[[1]]$updateTableBorderProperties$tableBorderProperties
  expect_equal(props$weight$magnitude, 1.5)
  expect_null(props$tableBorderFill$solidFill$alpha) # opaque, no alpha field
})

test_that("create_table_requests: plain table has borders key from default flextable borders", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_nob"
  )

  # Each cell emits all 4 sides directly (no deduplication of shared interior
  # borders). For a 4-row x 3-col table: 4 * 3 * 4 = 48 border requests.
  # Interior lines are written twice but are idempotent (transparent borders).
  expect_false(is.null(reqs$borders))
  expect_equal(length(reqs$borders$requests), 48L)
})

# -- r2slides_table S7 validation --

test_that("r2slides_table: rejects non-table_cell objects in cells list", {
  expect_error(
    r2slides_table(
      cells = list("not a cell"),
      n_rows = 1L,
      n_cols = 1L,
      header_rows = 0L
    ),
    regexp = "table_cell"
  )
})

test_that("r2slides_table: rejects n_rows < 1", {
  expect_error(
    r2slides_table(
      cells = list(),
      n_rows = 0L,
      n_cols = 1L,
      header_rows = 0L
    ),
    regexp = "positive"
  )
})

test_that("r2slides_table: rejects header_rows > n_rows", {
  cell <- table_cell(row_index = 0L, col_index = 0L)
  expect_error(
    r2slides_table(
      cells = list(cell),
      n_rows = 1L,
      n_cols = 1L,
      header_rows = 5L
    ),
    regexp = "header_rows"
  )
})

test_that("r2slides_table: rejects non-positive col_widths", {
  cell <- table_cell(row_index = 0L, col_index = 0L)
  expect_error(
    r2slides_table(
      cells = list(cell),
      n_rows = 1L,
      n_cols = 1L,
      col_widths = c(-1, 2),
      header_rows = 0L
    ),
    regexp = "positive"
  )
})

# -- table_cell S7 validation --

test_that("table_cell: rejects negative row_index or col_index", {
  expect_error(table_cell(row_index = -1L, col_index = 0L), regexp = ">= 0")
  expect_error(table_cell(row_index = 0L, col_index = -1L), regexp = ">= 0")
})

# -- cell_style S7 validation --

test_that("cell_style: rejects invalid v_align and non-positive spans", {
  expect_error(cell_style(v_align = "INVALID"), regexp = "v_align")
  expect_error(cell_style(col_span = 0L), regexp = "positive")
  expect_error(cell_style(row_span = 0L), regexp = "positive")
})

# -- as_r2slides_table: error handling --

test_that("as_r2slides_table: errors on unsupported input type", {
  expect_error(
    as_r2slides_table(list(a = 1)),
    class = "S7_error_method_not_found"
  )
})
