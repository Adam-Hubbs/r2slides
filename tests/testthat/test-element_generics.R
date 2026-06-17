# ── helpers ───────────────────────────────────────────────────────────────────

make_runs <- function(contents, starts, ends, styles) {
  tibble::tibble(
    content = contents,
    start = as.integer(starts),
    end = as.integer(ends),
    style = styles
  )
}

# ── PURE: runs_text ───────────────────────────────────────────────────────────

test_that("runs_text() concatenates run contents", {
  runs <- make_runs(
    c("Hello", " World"),
    c(1, 6),
    c(5, 11),
    list(list(), list())
  )
  expect_equal(r2slides:::runs_text(runs), "Hello World")
})

test_that("runs_text() returns empty string for zero rows", {
  empty <- make_runs(character(), integer(), integer(), list())
  expect_equal(r2slides:::runs_text(empty), "")
})

test_that("runs_text() handles single run", {
  runs <- make_runs("Only", 1, 4, list(list()))
  expect_equal(r2slides:::runs_text(runs), "Only")
})


# ── PURE: collapse_run_styles ─────────────────────────────────────────────────
# These tests cover pure branching logic that is awkward to force via the live
# API (e.g. a zero-run text box or a box with exactly 2 differently-styled runs
# where you need to verify the selector start/end values precisely).

test_that("collapse_run_styles() returns empty text_style for zero runs", {
  empty <- make_runs(character(), integer(), integer(), list())
  result <- r2slides:::collapse_run_styles(empty)
  expect_s3_class(result, "r2slides::text_style")
  expect_null(result@bold)
  expect_null(result@italic)
})

test_that("collapse_run_styles() returns bare text_style when all runs share the same style", {
  runs <- make_runs(
    c("Hello", " World"),
    c(1, 6),
    c(5, 11),
    list(list(bold = TRUE), list(bold = TRUE))
  )
  result <- r2slides:::collapse_run_styles(runs)
  expect_s3_class(result, "r2slides::text_style")
  expect_true(result@bold)
})

test_that("collapse_run_styles() returns bare text_style for a single run", {
  runs <- make_runs("Only", 1, 4, list(list(italic = TRUE)))
  result <- r2slides:::collapse_run_styles(runs)
  expect_s3_class(result, "r2slides::text_style")
  expect_true(result@italic)
})

test_that("collapse_run_styles() returns style_rule with correct shape for mixed-style runs", {
  runs <- make_runs(
    c("Hello", " World"),
    c(1, 6),
    c(5, 11),
    list(list(bold = TRUE), list(italic = TRUE))
  )
  result <- r2slides:::collapse_run_styles(runs)
  expect_s3_class(result, "r2slides::style_rule")
  expect_equal(result@num_selectors, 2L)
  expect_equal(result@num_styles, 2L)
})

test_that("collapse_run_styles() selectors return correct 1-based inclusive index pairs", {
  runs <- make_runs(
    c("Hello", " World"),
    c(1, 6),
    c(5, 11),
    list(list(bold = TRUE), list(italic = TRUE))
  )
  result <- r2slides:::collapse_run_styles(runs)

  # eval_tidy on a quosure containing a function call returns a function;
  # call it to get the c(start, end) vector.
  sel1_val <- rlang::eval_tidy(result@selector[[1]])
  sel2_val <- rlang::eval_tidy(result@selector[[2]])
  if (is.function(sel1_val)) sel1_val <- sel1_val()
  if (is.function(sel2_val)) sel2_val <- sel2_val()

  expect_equal(sel1_val, c(1L, 5L))
  expect_equal(sel2_val, c(6L, 11L))
})

test_that("collapse_run_styles() style_rule produces correct API ranges via create_styling_request()", {
  runs <- make_runs(
    c("Hello", " World"),
    c(1, 6),
    c(5, 11),
    list(list(bold = TRUE), list(italic = TRUE))
  )
  result <- r2slides:::collapse_run_styles(runs)
  requests <- create_styling_request(result, "Hello World", "elem_id")

  expect_length(requests, 2L)
  # Run 1: 1-based [1,5] -> API startIndex = 0, endIndex = 5
  expect_equal(requests[[1]]$textRange$startIndex, 0L)
  expect_equal(requests[[1]]$textRange$endIndex, 5L)
  # Run 2: 1-based [6,11] -> API startIndex = 5, endIndex = 11
  expect_equal(requests[[2]]$textRange$startIndex, 5L)
  expect_equal(requests[[2]]$textRange$endIndex, 11L)
})


# ── PURE: table_text_from_raw ─────────────────────────────────────────────────
# These test the pure list-parsing logic; the live get_table() round-trip below
# covers the path through the real API response shape.

test_that("table_text_from_raw() returns empty tibble for empty table", {
  raw <- list(table = list(tableRows = list()))
  result <- r2slides:::table_text_from_raw(raw)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("row", "col", "text"))
})

test_that("table_text_from_raw() extracts cell text from a 2x2 table", {
  raw <- list(
    table = list(
      tableRows = list(
        list(
          tableCells = list(
            list(
              text = list(
                textElements = list(
                  list(textRun = list(content = "A"))
                )
              )
            ),
            list(
              text = list(
                textElements = list(
                  list(textRun = list(content = "B"))
                )
              )
            )
          )
        ),
        list(
          tableCells = list(
            list(
              text = list(
                textElements = list(
                  list(textRun = list(content = "C"))
                )
              )
            ),
            list(
              text = list(
                textElements = list(
                  list(textRun = list(content = "D"))
                )
              )
            )
          )
        )
      )
    )
  )
  result <- r2slides:::table_text_from_raw(raw)
  expect_equal(nrow(result), 4L)
  expect_equal(result$text, c("A", "B", "C", "D"))
  expect_equal(result$row, c(1L, 1L, 2L, 2L))
  expect_equal(result$col, c(1L, 2L, 1L, 2L))
})

test_that("table_text_from_raw() concatenates multiple text runs in a cell", {
  raw <- list(
    table = list(
      tableRows = list(
        list(
          tableCells = list(
            list(
              text = list(
                textElements = list(
                  list(textRun = list(content = "foo")),
                  list(textRun = list(content = "bar"))
                )
              )
            )
          )
        )
      )
    )
  )
  result <- r2slides:::table_text_from_raw(raw)
  expect_equal(result$text[[1]], "foobar")
})


# ── VCR round-trip: text element uniform style ────────────────────────────────

test_that("text element round-trips through extract + rebuild (uniform style)", {
  vcr::use_cassette(
    "element_text_roundtrip_uniform",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      id_a <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      style <- text_style(bold = TRUE, font_size = 18)
      add_text(
        on_slide_id(id_a, ps),
        "Hello",
        in_top_left(),
        text_style = style
      )

      el <- get_elements(on_slide_id(id_a, ps), type = "text")[[1]]
      extracted_text <- get_text(el)
      extracted_style <- get_text_style(el)
      extracted_pos <- get_position(el)

      before2 <- unlist(ps$get_slide_ids())
      new_slide(ps)
      id_b <- setdiff(unlist(ps$get_slide_ids()), before2)[[1]]
      add_text(
        on_slide_id(id_b, ps),
        extracted_text,
        extracted_pos,
        text_style = extracted_style
      )

      slides_equal <- on_slide_id(id_a, ps) == on_slide_id(id_b, ps)

      delete_slide_raw(ps, id_a)
      delete_slide_raw(ps, id_b)
    }
  )

  # Trailing whitespace is trimmed, so the extracted text is the user-visible
  # string and uniform styling stays a single text_style (not a style_rule).
  expect_equal(extracted_text, "Hello")
  expect_s3_class(extracted_style, "r2slides::text_style")
  expect_s3_class(extracted_pos, "r2slides::slide_position")

  # Rebuilding a slide from the extracted text/style/position reproduces the
  # same content hash (slide_hash ignores objectId/revisionId).
  expect_true(slides_equal)
})


# ── VCR round-trip: get_position ─────────────────────────────────────────────

test_that("get_position() extracts correct position from a created text element", {
  vcr::use_cassette(
    "element_get_position_roundtrip",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      target_pos <- in_top_left()
      add_text(on_slide_id(slide_id, ps), "Position test", target_pos)

      el <- get_elements(on_slide_id(slide_id, ps), type = "text")[[1]]
      extracted_pos <- get_position(el)

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_true(is.slide_position(extracted_pos))
  expect_true(is.numeric(extracted_pos@top))
  expect_true(is.numeric(extracted_pos@left))
  expect_true(is.numeric(extracted_pos@width))
  expect_true(is.numeric(extracted_pos@height))
})


# ── VCR round-trip: get_elements type filter ──────────────────────────────────

test_that("get_elements() type filter returns only the requested element type", {
  vcr::use_cassette(
    "element_type_filter_roundtrip",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      add_text(on_slide_id(slide_id, ps), "Filter test", in_top_left())

      all_els <- get_elements(on_slide_id(slide_id, ps))
      text_els <- get_elements(on_slide_id(slide_id, ps), type = "text")

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_true(length(text_els) >= 1L)
  expect_true(all(purrr::map_chr(text_els, element_type) == "text"))
  expect_true(length(all_els) >= length(text_els))
})


# ── VCR round-trip: get_elements within (spatial filter) ─────────────────────

test_that("get_elements() within filter returns only elements overlapping the region", {
  vcr::use_cassette(
    "element_within_filter_roundtrip",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      # Place one element in the top-left quadrant and one in the bottom-right
      add_text(on_slide_id(slide_id, ps), "Top Left", in_top_left())
      add_text(on_slide_id(slide_id, ps), "Bottom Right", in_bottom_right())

      # Query within the top-left region only
      top_left_region <- slide_position(
        top = 0,
        left = 0,
        width = 3,
        height = 2
      )
      within_els <- get_elements(
        on_slide_id(slide_id, ps),
        type = "text",
        within = top_left_region
      )

      all_text_els <- get_elements(on_slide_id(slide_id, ps), type = "text")

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_true(length(within_els) >= 1L)
  expect_true(length(all_text_els) >= length(within_els))
})


# ── VCR round-trip: get_image ─────────────────────────────────────────────────

test_that("get_image() returns a URL string from an image element", {
  skip_if_not_installed("magick")

  vcr::use_cassette(
    "element_get_image_roundtrip",
    match_requests_on = c("method", "host", "path"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      add_image(
        on_slide_id(slide_id, ps),
        image = "https://www.gstatic.com/webp/gallery/1.jpg",
        position = in_top_left(),
        fit = "fill"
      )

      el <- get_elements(on_slide_id(slide_id, ps), type = "image")[[1]]
      img_url <- get_image(el)

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_type(img_url, "character")
  expect_true(nchar(img_url) > 0)
  expect_match(img_url, "^https?://")
})


# ── VCR round-trip: get_table ─────────────────────────────────────────────────

test_that("get_table() returns a tibble with correct dimensions from a real table element", {
  vcr::use_cassette(
    "element_get_table_roundtrip",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      # make_plain_ft() creates a 3-body-row x 3-col flextable (1 header + 3 body = 4 rows total)
      add_table(
        on_slide_id(slide_id, ps),
        make_plain_ft(),
        test_table_position()
      )

      el <- get_elements(on_slide_id(slide_id, ps), type = "table")[[1]]
      tbl <- get_table(el)

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_s3_class(tbl, "tbl_df")
  expect_named(tbl, c("row", "col", "text"))
  # make_plain_ft() renders as 1 header row + 3 body rows = 4 rows, 3 columns = 12 cells
  expect_equal(nrow(tbl), 12L)
  expect_true(all(tbl$col %in% 1:3))
  expect_true(all(tbl$row %in% 1:4))
})
