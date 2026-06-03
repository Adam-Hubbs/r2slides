# -- as_r2slides_table: plain flextable ----------------------------------------

test_that("as_r2slides_table: plain flextable has correct dimensions and cell count", {
  r2 <- as_r2slides_table(make_plain_ft())

  expect_equal(r2@n_rows, 4L) # 1 header + 3 body
  expect_equal(r2@n_cols, 3L)
  expect_equal(r2@header_rows, 1L)
  expect_length(r2@cells, 12L)
})

test_that("as_r2slides_table: cell indices are 0-based and cover the full grid", {
  r2 <- as_r2slides_table(make_plain_ft())

  row_indices <- purrr::map_int(r2@cells, \(c) c@row_index)
  col_indices <- purrr::map_int(r2@cells, \(c) c@col_index)

  expect_setequal(row_indices, 0L:3L)
  expect_setequal(col_indices, 0L:2L)
})

test_that("as_r2slides_table: header and body cell text is correctly extracted", {
  r2 <- as_r2slides_table(make_plain_ft())

  header_text <- r2@cells |>
    purrr::keep(\(c) c@row_index == 0L) |>
    purrr::map_chr(\(c) c@text %||% "")
  expect_setequal(header_text, c("a", "b", "c"))

  body_cell <- purrr::keep(
    r2@cells,
    \(c) c@row_index == 1L && c@col_index == 0L
  )[[1]]
  expect_equal(body_cell@text, "r1")
})

test_that("as_r2slides_table: col_widths and row_heights are positive and correctly sized", {
  r2 <- as_r2slides_table(make_plain_ft())

  expect_length(r2@col_widths, r2@n_cols)
  expect_length(r2@row_heights, r2@n_rows)
  expect_true(all(r2@col_widths > 0))
  expect_true(all(r2@row_heights > 0))
})

# -- as_r2slides_table: merged cells -------------------------------------------

test_that("as_r2slides_table: merged origin cell has correct row_span and col_span", {
  r2 <- as_r2slides_table(make_merged_ft())

  # merge_at(i = 1:2, j = 1) → body rows 1–2 in col 1 → row_index 1, col_index 0
  origin <- purrr::keep(r2@cells, \(c) c@row_index == 1L && c@col_index == 0L)[[
    1
  ]]

  expect_equal(origin@style@row_span, 2L)
  expect_equal(origin@style@col_span, 1L)
})

test_that("as_r2slides_table: consumed cells are flagged and non-merged cells are not", {
  r2 <- as_r2slides_table(make_merged_ft())

  consumed <- purrr::keep(
    r2@cells,
    \(c) c@row_index == 2L && c@col_index == 0L
  )[[1]]
  expect_true(consumed@consumed)

  regular <- purrr::keep(
    r2@cells,
    \(c) c@row_index == 1L && c@col_index == 1L
  )[[1]]
  expect_false(regular@consumed)
})

test_that("as_r2slides_table: non-merged cells have NULL span properties", {
  r2 <- as_r2slides_table(make_plain_ft())

  all_null <- purrr::every(
    r2@cells,
    \(c) is.null(c@style@row_span) && is.null(c@style@col_span)
  )
  expect_true(all_null)
})

# -- as_r2slides_table: borders ------------------------------------------------

test_that("as_r2slides_table: zero-width borders are stored as transparent with correct values", {
  r2 <- as_r2slides_table(make_plain_ft())

  cell <- purrr::keep(r2@cells, \(c) c@row_index == 2L && c@col_index == 1L)[[
    1
  ]]
  top <- cell@style@border_top

  # Width uses 1 PT in EMU (12700) as a placeholder so the Slides API accepts it;
  # alpha = 0 makes it visually invisible.
  expect_equal(top$width, 12700)
  expect_equal(top$dash_style, "SOLID")
  expect_true(S7::S7_inherits(top$color, r2s_color))
  expect_equal(top$color@alpha, 0)
})

test_that("as_r2slides_table: visible borders have positive EMU width and no alpha", {
  r2 <- as_r2slides_table(make_bordered_ft())

  # Header row (row_index 0) has a 3 PT visible red top border
  hdr_cell <- purrr::keep(
    r2@cells,
    \(c) c@row_index == 0L && c@col_index == 0L
  )[[1]]
  top <- hdr_cell@style@border_top

  expect_true(S7::S7_inherits(top$color, r2s_color))
  expect_null(top$color@alpha) # opaque: alpha not set
  expect_gt(top$width, 12700) # wider than the transparent placeholder
})

test_that("as_r2slides_table: dashed borders map to DASH style", {
  r2 <- as_r2slides_table(make_borders_ft())

  # make_borders_ft: 1 header + body, "i = 3" in body → row_index = 3
  dashed_cell <- purrr::keep(
    r2@cells,
    \(c) c@row_index == 3L && c@col_index == 0L
  )[[1]]
  expect_equal(dashed_cell@style@border_bottom$dash_style, "DASH")
})

# -- create_table_requests: structure ------------------------------------------

test_that("create_table_requests: returns expected top-level keys", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_1"
  )

  expect_in(c("create", "cells", "row_heights"), names(reqs))
  expect_false("merges" %in% names(reqs))
})

test_that("create_table_requests: createTable request has correct structure", {
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

test_that("create_table_requests: insertText requests cover header and body cell text", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_txt"
  )

  inserted <- reqs$cells$requests |>
    purrr::keep(\(r) !is.null(r$insertText)) |>
    purrr::map_chr(\(r) r$insertText$text)

  expect_in("a", inserted)
  expect_in("r1", inserted)
})

# -- create_table_requests: merges ---------------------------------------------

test_that("create_table_requests: merged table produces a mergeTableCells request", {
  r2 <- as_r2slides_table(make_merged_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_mg"
  )

  expect_false(is.null(reqs$merges))

  merge <- reqs$merges$requests[[1]]$mergeTableCells
  expect_equal(merge$tableRange$location$rowIndex, 1L)
  expect_equal(merge$tableRange$location$columnIndex, 0L)
  expect_equal(merge$tableRange$rowSpan, 2L)
  expect_equal(merge$tableRange$columnSpan, 1L)
})

test_that("create_table_requests: plain table has no merges key", {
  r2 <- as_r2slides_table(make_plain_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_nom"
  )

  expect_null(reqs$merges)
})

# -- create_table_requests: borders --------------------------------------------

test_that("create_table_requests: visible borders produce border requests", {
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

test_that("create_table_requests: transparent border requests come before visible ones", {
  r2 <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_ord"
  )

  is_transparent <- purrr::map_lgl(reqs$borders$requests, \(r) {
    alpha <- r$updateTableBorderProperties$tableBorderProperties$tableBorderFill$solidFill$alpha
    !is.null(alpha) && alpha == 0
  })

  last_transparent <- max(c(0L, which(is_transparent)))
  first_visible <- min(c(Inf, which(!is_transparent)))
  expect_lte(last_transparent, first_visible)
})

test_that("create_table_requests: visible border request has opaque fill and positive weight", {
  r2 <- as_r2slides_table(make_bordered_ft())
  reqs <- create_table_requests(
    r2,
    "slide_abc",
    test_table_position(),
    table_id = "tbl_rb"
  )

  top_reqs <- find_border_req(reqs$borders$requests, 0L, 0L, "TOP")
  props <- top_reqs[[1]]$updateTableBorderProperties$tableBorderProperties

  expect_null(props$tableBorderFill$solidFill$alpha) # opaque: no alpha field
  expect_gt(props$weight$magnitude, 0)
})

# -- as_r2slides_table: error handling -----------------------------------------

test_that("as_r2slides_table: errors on unsupported input type", {
  expect_snapshot(error = TRUE, as_r2slides_table(list(a = 1)))
})
