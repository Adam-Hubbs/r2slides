# ── set/get_replacement_strategy ─────────────────────────────────────────────

test_that("get_replacement_strategy() returns 'add' by default", {
  withr::local_envvar(r2slides_replacement_strategy = NA_character_)
  expect_equal(get_replacement_strategy(), "add")
})

test_that("set_replacement_strategy() round-trips all valid values", {
  withr::local_envvar(r2slides_replacement_strategy = NA_character_)
  for (s in c("add", "replace", "skip")) {
    set_replacement_strategy(s)
    expect_equal(get_replacement_strategy(), s)
  }
})

test_that("set_replacement_strategy() rejects unknown values", {
  expect_snapshot(error = TRUE, set_replacement_strategy("upsert"))
})

test_that("get_replacement_strategy() warns on unknown env var and returns 'add'", {
  withr::local_envvar(r2slides_replacement_strategy = "turbo")
  expect_snapshot(result <- get_replacement_strategy())
  expect_equal(result, "add")
})

# ── set/get_match_fn ──────────────────────────────────────────────────────────

test_that("set_match_fn() rejects non-functions", {
  expect_snapshot(error = TRUE, set_match_fn("not_a_function"))
})


# ── detect_element_type ─────────────────────────────────────────────────────

test_that("detect_element_type() identifies TEXT_BOX", {
  el <- list(
    objectId = "e1",
    shape = list(shapeType = "TEXT_BOX"),
    transform = list(translateX = 0, translateY = 0),
    size = list(width = list(magnitude = 0), height = list(magnitude = 0))
  )
  expect_equal(detect_element_type(el), "TEXT_BOX")
})

test_that("detect_element_type() treats non-TEXT_BOX shapes as UNSUPPORTED", {
  el <- list(
    shape = list(shapeType = "RECTANGLE"),
    transform = list(),
    size = list()
  )
  expect_equal(detect_element_type(el), "UNSUPPORTED")
})

test_that("detect_element_type() identifies SHEETS_CHART", {
  el <- list(sheetsChart = list(), transform = list(), size = list())
  expect_equal(detect_element_type(el), "SHEETS_CHART")
})

test_that("detect_element_type() identifies IMAGE", {
  el <- list(image = list(), transform = list(), size = list())
  expect_equal(detect_element_type(el), "IMAGE")
})

test_that("detect_element_type() identifies TABLE", {
  el <- list(table = list(), transform = list(), size = list())
  expect_equal(detect_element_type(el), "TABLE")
})

test_that("detect_element_type() returns UNSUPPORTED for unknown element", {
  el <- list(wordArt = list(), transform = list(), size = list())
  expect_equal(detect_element_type(el), "UNSUPPORTED")
})

# ── match_by_type_and_position ────────────────────────────────────────────────

make_elements <- function(...) {
  rows <- list(...)
  tibble::tibble(
    element_id = purrr::map_chr(rows, "element_id"),
    type = purrr::map_chr(rows, "type"),
    left = purrr::map_dbl(rows, "left"),
    top = purrr::map_dbl(rows, "top"),
    width = purrr::map_dbl(rows, "width"),
    height = purrr::map_dbl(rows, "height")
  )
}

test_that("match_by_type_and_position() returns NULL for empty existing elements", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  existing <- tibble::tibble(
    element_id = character(),
    type = character(),
    left = numeric(),
    top = numeric(),
    width = numeric(),
    height = numeric()
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1,
    top = 1,
    width = 2,
    height = 0.5
  )
  expect_null(matcher(new_spec, existing))
})

test_that("match_by_type_and_position() returns NULL when no type match", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "IMAGE",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1,
    top = 1,
    width = 2,
    height = 0.5
  )
  expect_null(matcher(new_spec, existing))
})

test_that("match_by_type_and_position() matches exact centroid", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1,
    top = 1,
    width = 2,
    height = 0.5
  )
  expect_equal(matcher(new_spec, existing), "e1")
})

test_that("match_by_type_and_position() matches within tolerance", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  # existing centroid: (1 + 2/2, 1 + 0.5/2) = (2, 1.25)
  # new centroid: (2 + 0.04, 1.25 + 0.04) — both within 0.05
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1.04,
    top = 1.04,
    width = 2,
    height = 0.5
  )
  expect_equal(matcher(new_spec, existing), "e1")
})

test_that("match_by_type_and_position() returns NULL outside tolerance", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1.5,
    top = 1.5,
    width = 2,
    height = 0.5
  )
  expect_null(matcher(new_spec, existing))
})

test_that("match_by_type_and_position() warns and returns NULL on multiple matches", {
  matcher <- match_by_type_and_position(tolerance = 0.5)
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    ),
    list(
      element_id = "e2",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1,
    top = 1,
    width = 2,
    height = 0.5
  )
  expect_snapshot(result <- matcher(new_spec, existing))
  expect_null(result)
})

test_that("match_by_type_and_position() respects custom tolerance", {
  tight <- match_by_type_and_position(tolerance = 0.01)
  loose <- match_by_type_and_position(tolerance = 0.5)
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  # centroid offset of 0.1 — inside loose, outside tight
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1.1,
    top = 1.1,
    width = 2,
    height = 0.5
  )
  expect_null(tight(new_spec, existing))
  expect_equal(loose(new_spec, existing), "e1")
})

test_that("match_by_type_and_position() ignores non-matching types among multiple elements", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  existing <- make_elements(
    list(
      element_id = "img",
      type = "IMAGE",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    ),
    list(
      element_id = "tbl",
      type = "TABLE",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    ),
    list(
      element_id = "txt",
      type = "TEXT_BOX",
      left = 1,
      top = 1,
      width = 2,
      height = 0.5
    )
  )
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1,
    top = 1,
    width = 2,
    height = 0.5
  )
  expect_equal(matcher(new_spec, existing), "txt")
})

test_that("match_by_type_and_position() uses centroid not top-left for matching", {
  matcher <- match_by_type_and_position(tolerance = 0.05)
  # Same centroid, different top-left and size
  existing <- make_elements(
    list(
      element_id = "e1",
      type = "TEXT_BOX",
      left = 0,
      top = 0,
      width = 4,
      height = 2
    )
  )
  # Centroid of existing: (2.0, 1.0); new element also centred at (2.0, 1.0) but placed differently
  new_spec <- list(
    type = "TEXT_BOX",
    left = 1,
    top = 0.5,
    width = 2,
    height = 1
  )
  expect_equal(matcher(new_spec, existing), "e1")
})

# ── .normalize_page_element ───────────────────────────────────────────────────

test_that(".normalize_page_element() extracts correct position with identity scale", {
  emu <- 914400L
  el <- make_raw_el(
    "e1",
    "TEXT_BOX",
    0.12 * emu,
    1.48 * emu,
    3.2 * emu,
    1.8 * emu
  )
  row <- .normalize_page_element(el)
  expect_equal(row$left, 0.12, tolerance = 1e-6)
  expect_equal(row$top, 1.48, tolerance = 1e-6)
  expect_equal(row$width, 3.2, tolerance = 1e-6)
  expect_equal(row$height, 1.8, tolerance = 1e-6)
})

test_that(".normalize_page_element() applies scaleX/scaleY to dimensions (Google normalisation)", {
  # Google stored (size=3000000, scaleX=0.9754, scaleY=0.5486) for a 3.2×1.8 inch element
  emu <- 914400L
  el <- make_raw_el(
    "e1",
    "TEXT_BOX",
    0.12 * emu,
    1.48 * emu,
    3000000,
    3000000,
    scale_x = 0.9754,
    scale_y = 0.5486
  )
  row <- .normalize_page_element(el)
  expect_equal(row$left, 0.12, tolerance = 1e-6)
  expect_equal(row$top, 1.48, tolerance = 1e-6)
  expect_equal(row$width, 3000000 * 0.9754 / emu, tolerance = 1e-6) # ≈ 3.2
  expect_equal(row$height, 3000000 * 0.5486 / emu, tolerance = 1e-6) # ≈ 1.8
})

test_that("match_by_type_and_position() matches Google-normalised element against spec", {
  # The bug: without scale fix, extracted height was 3.28 instead of 1.8,
  # making the centroid 0.74 inches off and failing the 0.05 tolerance.
  matcher <- match_by_type_and_position(tolerance = 0.05)
  emu <- 914400L
  el <- make_raw_el(
    "text_abc",
    "TEXT_BOX",
    0.12 * emu,
    1.48 * emu,
    3000000,
    3000000,
    scale_x = 0.9754,
    scale_y = 0.5486
  )
  existing <- purrr::list_rbind(list(.normalize_page_element(el)))
  new_spec <- list(
    type = "TEXT_BOX",
    left = 0.12,
    top = 1.48,
    width = 3.2,
    height = 1.8
  )
  expect_equal(matcher(new_spec, existing), "text_abc")
})

# ── Integration: get_page_elements + add_text replace/skip ───────────────────

test_that("get_page_elements() returns a tibble with correct columns", {
  vcr::use_cassette(
    "replacement_get_page_elements",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]
      slide_obj <- on_slide_id(test_slide_id, ps)

      add_text(slide_obj, "Hello", in_top_left())
      elements <- get_page_elements(slide_obj)

      delete_slide_raw(ps, test_slide_id)
    }
  )

  expect_s3_class(elements, "tbl_df")
  expect_named(
    elements,
    c("element_id", "type", "left", "top", "width", "height")
  )
  expect_gte(nrow(elements), 1L)
  expect_true("TEXT_BOX" %in% elements$type)
})

test_that("add_text() with strategy 'replace' removes the old element", {
  vcr::use_cassette(
    "replacement_add_text_replace",
    match_requests_on = c("method", "uri"),
    {
      withr::local_envvar(r2slides_replacement_strategy = "replace")

      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]
      slide_obj <- on_slide_id(test_slide_id, ps)

      add_text(slide_obj, "First", in_top_left())
      elements_after_first <- get_page_elements(slide_obj)

      add_text(slide_obj, "Second", in_top_left())
      elements_after_second <- get_page_elements(slide_obj)

      delete_slide_raw(ps, test_slide_id)
    }
  )

  first_text_boxes <- dplyr::filter(elements_after_first, type == "TEXT_BOX")
  second_text_boxes <- dplyr::filter(elements_after_second, type == "TEXT_BOX")

  expect_equal(nrow(first_text_boxes), 1L)
  expect_equal(nrow(second_text_boxes), 1L)
  expect_false(
    first_text_boxes$element_id[[1]] == second_text_boxes$element_id[[1]]
  )
})

test_that("add_text() with strategy 'skip' leaves existing element untouched", {
  vcr::use_cassette(
    "replacement_add_text_skip",
    match_requests_on = c("method", "uri"),
    {
      withr::local_envvar(r2slides_replacement_strategy = "skip")

      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]
      slide_obj <- on_slide_id(test_slide_id, ps)

      add_text(slide_obj, "First", in_top_left())
      elements_after_first <- get_page_elements(slide_obj)

      expect_message(
        add_text(slide_obj, "Second", in_top_left()),
        "Skipping"
      )
      elements_after_second <- get_page_elements(slide_obj)

      delete_slide_raw(ps, test_slide_id)
    }
  )

  first_text_boxes <- dplyr::filter(elements_after_first, type == "TEXT_BOX")
  second_text_boxes <- dplyr::filter(elements_after_second, type == "TEXT_BOX")

  expect_equal(nrow(second_text_boxes), nrow(first_text_boxes))
  expect_equal(
    first_text_boxes$element_id[[1]],
    second_text_boxes$element_id[[1]]
  )
})
