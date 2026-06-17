# Pull internal geometry helpers into the test environment so they are
# unit-testable without the API.
boxes_overlap <- r2slides:::boxes_overlap
box_contains <- r2slides:::box_contains
point_in_box <- r2slides:::point_in_box

# ── PURE: boxes_overlap() ─────────────────────────────────────────────────────

make_box <- function(top, left, bottom, right) {
  list(top = top, left = left, bottom = bottom, right = right)
}

test_that("boxes_overlap: clearly overlapping boxes return TRUE", {
  a <- make_box(0, 0, 4, 4)
  b <- make_box(2, 2, 6, 6)
  expect_true(boxes_overlap(a, b))
  expect_true(boxes_overlap(b, a))
})

test_that("boxes_overlap: identical boxes overlap", {
  a <- make_box(1, 1, 3, 3)
  expect_true(boxes_overlap(a, a))
})

test_that("boxes_overlap: clearly disjoint boxes return FALSE", {
  a <- make_box(0, 0, 2, 2)
  b <- make_box(5, 5, 8, 8)
  expect_false(boxes_overlap(a, b))
  expect_false(boxes_overlap(b, a))
})

test_that("boxes_overlap: horizontally disjoint (touching edges) return FALSE", {
  a <- make_box(0, 0, 2, 2)
  b <- make_box(0, 2, 2, 4)
  # touching edges (right == left): NOT overlapping
  expect_false(boxes_overlap(a, b))
})

test_that("boxes_overlap: vertically disjoint (touching edges) return FALSE", {
  a <- make_box(0, 0, 2, 2)
  b <- make_box(2, 0, 4, 2)
  # touching edges (bottom == top): NOT overlapping
  expect_false(boxes_overlap(a, b))
})

test_that("boxes_overlap: partial horizontal overlap", {
  a <- make_box(0, 0, 4, 3)
  b <- make_box(0, 2, 4, 6)
  expect_true(boxes_overlap(a, b))
})

test_that("boxes_overlap: one box inside another returns TRUE", {
  outer <- make_box(0, 0, 10, 10)
  inner <- make_box(2, 2, 5, 5)
  expect_true(boxes_overlap(outer, inner))
  expect_true(boxes_overlap(inner, outer))
})

# ── PURE: box_contains() ──────────────────────────────────────────────────────

test_that("box_contains: inner fully inside outer returns TRUE", {
  outer <- make_box(0, 0, 10, 10)
  inner <- make_box(1, 1, 9, 9)
  expect_true(box_contains(outer, inner))
})

test_that("box_contains: inner equal to outer returns TRUE", {
  box <- make_box(0, 0, 5, 5)
  expect_true(box_contains(box, box))
})

test_that("box_contains: inner overlapping but not contained returns FALSE", {
  outer <- make_box(0, 0, 5, 5)
  inner <- make_box(3, 3, 7, 7)
  expect_false(box_contains(outer, inner))
})

test_that("box_contains: disjoint boxes return FALSE", {
  outer <- make_box(0, 0, 3, 3)
  inner <- make_box(5, 5, 8, 8)
  expect_false(box_contains(outer, inner))
})

test_that("box_contains: inner touching left edge is contained", {
  outer <- make_box(0, 0, 5, 5)
  inner <- make_box(0, 0, 3, 3)
  expect_true(box_contains(outer, inner))
})

test_that("box_contains: inner extending beyond one edge is not contained", {
  outer <- make_box(1, 1, 5, 5)
  inner <- make_box(0, 1, 5, 5) # inner top is 0, outer top is 1
  expect_false(box_contains(outer, inner))
})

test_that("box_contains: outer fully inside inner returns FALSE", {
  outer <- make_box(2, 2, 4, 4)
  inner <- make_box(0, 0, 10, 10)
  expect_false(box_contains(outer, inner))
})

# ── PURE: point_in_box() ──────────────────────────────────────────────────────

test_that("point_in_box: point clearly inside returns TRUE", {
  box <- make_box(0, 0, 4, 4)
  expect_true(point_in_box(box, 2, 2))
})

test_that("point_in_box: point clearly outside returns FALSE", {
  box <- make_box(0, 0, 2, 2)
  expect_false(point_in_box(box, 5, 5))
})

test_that("point_in_box: point on the edge is inside", {
  box <- make_box(1, 1, 3, 3)
  expect_true(point_in_box(box, 1, 1))
  expect_true(point_in_box(box, 3, 3))
})

test_that("point_in_box: point outside on a single axis returns FALSE", {
  box <- make_box(0, 0, 4, 4)
  expect_false(point_in_box(box, 2, 5)) # left beyond right edge
  expect_false(point_in_box(box, 5, 2)) # top beyond bottom edge
})


# ── PURE: get_elements() validation errors ────────────────────────────────────

test_that("get_elements errors when passed a non-slide object", {
  expect_snapshot(
    error = TRUE,
    get_elements(list())
  )
})

test_that("get_elements errors on invalid type values", {
  expect_snapshot(
    error = TRUE,
    get_elements(list(), type = "invalid_type")
  )

  expect_snapshot(
    error = TRUE,
    get_elements(list(), type = c("text", "not_a_type"))
  )
})

test_that("get_elements errors when within is wrong type", {
  expect_snapshot(
    error = TRUE,
    get_elements(list(), within = "not a position")
  )

  expect_snapshot(
    error = TRUE,
    get_elements(list(), within = c(1, 2, 3))
  )
})


# ── VCR: get_elements() returns empty list for a blank slide ──────────────────

test_that("get_elements() returns empty list for a blank new slide", {
  vcr::use_cassette(
    "get_elements_empty_slide",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      elements <- get_elements(on_slide_id(slide_id, ps))

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_type(elements, "list")
  expect_length(elements, 0L)
})


# ── VCR: get_elements() with contains = "all" ─────────────────────────────────

test_that("get_elements() contains = 'all' filters to fully-contained elements only", {
  vcr::use_cassette(
    "get_elements_contains_all",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      # Place a small element fully inside the top-left region
      inner_pos <- slide_position(
        top = 0.1,
        left = 0.1,
        width = 1,
        height = 0.5
      )
      add_text(on_slide_id(slide_id, ps), "Inside", inner_pos)

      region <- slide_position(top = 0, left = 0, width = 3, height = 2)

      els_any <- get_elements(
        on_slide_id(slide_id, ps),
        type = "text",
        within = region,
        contains = "any"
      )

      els_all <- get_elements(
        on_slide_id(slide_id, ps),
        type = "text",
        within = region,
        contains = "all"
      )

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_true(length(els_all) >= 1L)
  expect_true(length(els_any) >= length(els_all))
})


# ── VCR: get_elements() with a c(top, left) point ─────────────────────────────

test_that("get_elements() with a point returns elements covering that point", {
  vcr::use_cassette(
    "get_elements_within_point",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      # An element occupying roughly the box top 0.5..1.5, left 0.5..2.5
      pos <- slide_position(top = 0.5, left = 0.5, width = 2, height = 1)
      add_text(on_slide_id(slide_id, ps), "Target", pos)

      hit <- get_elements(on_slide_id(slide_id, ps), within = c(1, 1))
      miss <- get_elements(on_slide_id(slide_id, ps), within = c(4, 4))

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_true(length(hit) >= 1L)
  expect_length(miss, 0L)
})
