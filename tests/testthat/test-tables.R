test_that("as_r2slides_table: plain flextable has correct dimensions and cell count", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  # 1 header row + 3 body rows, 3 cols
  expect_s3_class(r2, "r2slides::r2slides_table")
  expect_equal(r2@n_rows,      4L)
  expect_equal(r2@n_cols,      3L)
  expect_equal(r2@header_rows, 1L)
  expect_equal(length(r2@cells), 12L)
})

test_that("as_r2slides_table: cell indices are 0-based and cover the full grid", {
  ft  <- make_plain_ft()
  r2  <- as_r2slides_table(ft)

  row_indices <- vapply(r2@cells, \(c) c@row_index, integer(1))
  col_indices <- vapply(r2@cells, \(c) c@col_index, integer(1))

  expect_equal(sort(unique(row_indices)), 0L:3L)
  expect_equal(sort(unique(col_indices)), 0L:2L)
})

test_that("as_r2slides_table: header cell text matches column names", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  header_cells <- Filter(\(c) c@row_index == 0L, r2@cells)
  header_text  <- vapply(header_cells, \(c) c@text %||% "", character(1))

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

  expect_length(r2@col_widths,  r2@n_cols)
  expect_length(r2@row_heights, r2@n_rows)
  expect_true(all(r2@col_widths  > 0))
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

  all_null <- all(vapply(r2@cells, \(c) {
    is.null(c@style@row_span) && is.null(c@style@col_span)
  }, logical(1)))

  expect_true(all_null)
})

# -- Borders --

test_that("as_r2slides_table: explicit border colour and width are captured", {
  ft <- make_bordered_ft()
  r2 <- as_r2slides_table(ft)

  cell_top <- Filter(\(c) c@row_index == 1L && c@col_index == 0L, r2@cells)[[1]]
  expect_equal(cell_top@style@border_top$color, "#FF0000")
  expect_equal(cell_top@style@border_top$width, 2)

  cell_bot <- Filter(\(c) c@row_index == 2L && c@col_index == 0L, r2@cells)[[1]]
  expect_equal(cell_bot@style@border_bottom$color, "#0000FF")
  expect_equal(cell_bot@style@border_bottom$width, 1)
  expect_equal(cell_bot@style@border_bottom$dash_style, "DASH")
})

test_that("as_r2slides_table: cells without explicit borders carry only default border colour", {
  ft <- make_plain_ft()
  r2 <- as_r2slides_table(ft)

  # Flextable zero-width borders cannot use weight=0 in the Google Slides API,
  # so they are emitted as width=1 / color=#FFFFFF (white = visually invisible)
  # to override the Google Slides default visible border.
  some_cell <- Filter(\(c) c@row_index == 2L && c@col_index == 1L, r2@cells)[[1]]
  expect_equal(some_cell@style@border_top$width, 1)
  expect_equal(some_cell@style@border_top$color, "#FFFFFF")
  expect_equal(some_cell@style@border_top$dash_style, "SOLID")
  expect_equal(some_cell@style@border_bottom$width, 1)
  expect_equal(some_cell@style@border_bottom$color, "#FFFFFF")
  # default colour is black, not a custom colour
  expect_false(identical(some_cell@style@border_top$color, "#FF0000"))
})

# -- create_table_requests: structure --

test_that("create_table_requests: returns expected top-level keys", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_1")

  expect_named(reqs, c("create", "col_widths", "cells", "borders", "row_heights"), ignore.order = TRUE)
})

test_that("create_table_requests: createTable request has correct dimensions and id", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_plain")

  ct <- reqs$create$requests[[1]]$createTable
  expect_equal(ct$objectId, "tbl_plain")
  expect_equal(ct$rows,     4L)
  expect_equal(ct$columns,  3L)
  expect_equal(ct$elementProperties$pageObjectId, "slide_abc")
})

test_that("create_table_requests: col_widths has one request per column", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_cw")

  expect_length(reqs$col_widths$requests, r2@n_cols)
})

test_that("create_table_requests: row_heights has one request per row", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_rh")

  expect_length(reqs$row_heights$requests, r2@n_rows)
})

test_that("create_table_requests: insertText requests match non-empty cells", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_txt")

  text_reqs <- Filter(\(r) !is.null(r$insertText), reqs$cells$requests)
  inserted  <- vapply(text_reqs, \(r) r$insertText$text, character(1))

  expect_true("r1" %in% inserted)
  expect_true("a"  %in% inserted)
})

# -- create_table_requests: merges --

test_that("create_table_requests: merge produces a mergeTableCells request", {
  r2   <- as_r2slides_table(make_merged_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_mg")

  expect_false(is.null(reqs$merges))
  expect_length(reqs$merges$requests, 1L)

  merge <- reqs$merges$requests[[1]]$mergeTableCells
  expect_equal(merge$tableRange$location$rowIndex,    1L)
  expect_equal(merge$tableRange$location$columnIndex, 0L)
  expect_equal(merge$tableRange$rowSpan,              2L)
  expect_equal(merge$tableRange$columnSpan,           1L)
})

test_that("create_table_requests: no merges key when table has no merged cells", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_nom")

  expect_null(reqs$merges)
})

# -- create_table_requests: borders --

test_that("create_table_requests: explicit borders produce border requests", {
  r2   <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_bd")

  expect_false(is.null(reqs$borders))
  expect_gt(length(reqs$borders$requests), 0L)
})

test_that("create_table_requests: red top border request is correct", {
  r2   <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_rb")

  # border.top on body row 1 is the same physical line as BOTTOM of header row 0.
  # The canonicalisation emits it from the owner cell (row 0) as BOTTOM.
  red_reqs <- find_border_req(reqs$borders$requests, 0L, 0L, "BOTTOM")
  expect_length(red_reqs, 1L)

  props <- red_reqs[[1]]$updateTableBorderProperties$tableBorderProperties
  rgb   <- props$tableBorderFill$solidFill$color$rgbColor
  expect_equal(rgb$red,   1)
  expect_equal(rgb$green, 0)
  expect_equal(rgb$blue,  0)
  expect_equal(props$weight$magnitude, 2)
  expect_equal(props$dashStyle, "SOLID")
})

test_that("create_table_requests: dashed blue bottom border request is correct", {
  r2   <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_bb")

  blue_reqs <- find_border_req(reqs$borders$requests, 2L, 0L, "BOTTOM")
  expect_length(blue_reqs, 1L)

  props <- blue_reqs[[1]]$updateTableBorderProperties$tableBorderProperties
  rgb   <- props$tableBorderFill$solidFill$color$rgbColor
  expect_equal(rgb$red,   0)
  expect_equal(rgb$green, 0)
  expect_equal(rgb$blue,  1)
  expect_equal(props$weight$magnitude, 1)
  expect_equal(props$dashStyle, "DASH")
})

test_that("create_table_requests: plain table has borders key from default flextable borders", {
  r2   <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(r2, "slide_abc", test_table_position(), table_id = "tbl_nob")

  # Each physical border line is emitted exactly once (shared interior borders
  # are canonicalised to the owner cell). For a 4-row x 3-col table:
  #   horizontal lines: (4+1) * 3 = 15
  #   vertical lines:    4 * (3+1) = 16
  #   total: 31 unique border requests
  expect_false(is.null(reqs$borders))
  expect_equal(length(reqs$borders$requests), 31L)
})

# -- r2slides_table S7 validation --

test_that("r2slides_table: rejects non-table_cell objects in cells list", {
  expect_error(
    r2slides_table(
      cells       = list("not a cell"),
      n_rows      = 1L,
      n_cols      = 1L,
      header_rows = 0L
    ),
    regexp = "table_cell"
  )
})

test_that("r2slides_table: rejects n_rows < 1", {
  expect_error(
    r2slides_table(
      cells       = list(),
      n_rows      = 0L,
      n_cols      = 1L,
      header_rows = 0L
    ),
    regexp = "positive"
  )
})

test_that("r2slides_table: rejects header_rows > n_rows", {
  cell <- table_cell(row_index = 0L, col_index = 0L)
  expect_error(
    r2slides_table(
      cells       = list(cell),
      n_rows      = 1L,
      n_cols      = 1L,
      header_rows = 5L
    ),
    regexp = "header_rows"
  )
})

test_that("r2slides_table: rejects non-positive col_widths", {
  cell <- table_cell(row_index = 0L, col_index = 0L)
  expect_error(
    r2slides_table(
      cells       = list(cell),
      n_rows      = 1L,
      n_cols      = 1L,
      col_widths  = c(-1, 2),
      header_rows = 0L
    ),
    regexp = "positive"
  )
})

# -- table_cell S7 validation --

test_that("table_cell: rejects negative row_index or col_index", {
  expect_error(table_cell(row_index = -1L, col_index =  0L), regexp = ">= 0")
  expect_error(table_cell(row_index =  0L, col_index = -1L), regexp = ">= 0")
})

# -- cell_style S7 validation --

test_that("cell_style: rejects invalid v_align and non-positive spans", {
  expect_error(cell_style(v_align  = "INVALID"), regexp = "v_align")
  expect_error(cell_style(col_span = 0L),        regexp = "positive")
  expect_error(cell_style(row_span = 0L),        regexp = "positive")
})

# -- as_r2slides_table: error handling --

test_that("as_r2slides_table: errors on unsupported input type", {
  expect_error(as_r2slides_table(list(a = 1)), class = "S7_error_method_not_found")
})
