test_that("add_text() adds a text element and records it in the ledger", {
  vcr::use_cassette(
    "add_text_basic",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)
      add_text(slide_obj, "Hello World", in_top_left(), verbose = FALSE)

      delete_slide_raw(ps, test_slide_id)
    }
  )

  elements <- ps$get_elements()
  expect_false(is.null(elements))
  expect_equal(elements[[1]]$slide_id, test_slide_id)
  expect_equal(
    ps$get_elements(element_type = "text")[[1]]$slide_id,
    test_slide_id
  )
})

test_that("add_text_multi() adds multiple text elements to the same slide", {
  vcr::use_cassette(
    "add_text_multi_basic",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)
      add_text_multi(
        slide_obj,
        text = c("First", "Second"),
        position = list(in_top_left(), in_top_right()),
        verbose = FALSE
      )

      delete_slide_raw(ps, test_slide_id)
    }
  )

  elements <- ps$get_elements(element_type = "text")
  expect_false(is.null(elements))
  expect_length(elements, 2L)
  expect_true(all(purrr::map_chr(elements, "slide_id") == test_slide_id))
})

test_that("add_text() errors on non-character text", {
  vcr::use_cassette(
    "add_text_errors",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]
      slide_obj <- on_slide_id(test_slide_id, ps)
      delete_slide_raw(ps, test_slide_id)
    }
  )

  expect_snapshot(error = TRUE, add_text(slide_obj, 123, in_top_left()))
  expect_snapshot(
    error = TRUE,
    add_text(slide_obj, "text", list(top = 1))
  )
})

# ── get_safe_length (no API) ──────────────────────────────────────────────────

test_that("get_safe_length() returns correct lengths for common argument types", {
  expect_equal(get_safe_length(NULL), 0L)
  expect_equal(
    get_safe_length(function() {
    }),
    1L
  )
  expect_equal(get_safe_length(identity), 1L)
  expect_equal(get_safe_length("a"), 1L)
  expect_equal(get_safe_length(c(1, 2, 3)), 3L)
  expect_equal(get_safe_length(list(1, 2)), 2L)
  expect_equal(get_safe_length(in_top_left()), 1L)
})

test_that("get_safe_length() gives a helpful error when a called position function is passed", {
  expect_snapshot(
    error = TRUE,
    get_safe_length(relative_position())
  )
})

# ── add_text_multi() argument recycling and errors (no API for errors) ────────

test_that("add_text_multi() errors when argument lengths cannot be recycled", {
  expect_snapshot(
    error = TRUE,
    add_text_multi(
      slide_obj = NULL,
      text = c("A", "B", "C"),
      position = list(in_top_left(), in_top_right()),
      verbose = FALSE
    )
  )
})

test_that("add_text_multi() recycles scalar text across multiple positions", {
  vcr::use_cassette(
    "add_text_multi_recycling",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)
      add_text_multi(
        slide_obj,
        text = "Same Text",
        position = list(in_top_left(), in_top_middle(), in_top_right()),
        verbose = FALSE
      )

      delete_slide_raw(ps, test_slide_id)
    }
  )

  elements <- ps$get_elements(element_type = "text")
  expect_length(elements, 3L)
  expect_true(all(purrr::map_chr(elements, "slide_id") == test_slide_id))
})

test_that("add_text_multi() accepts relative transformation functions as position", {
  vcr::use_cassette(
    "add_text_multi_position_func",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)

      # Shift the base position right by 1 inch for each element
      shift_right <- define_relative_transformation_function(
        left_transformation = \(x) x + 1
      )

      add_text_multi(
        slide_obj,
        text = c("A", "B"),
        position = shift_right,
        position_base = list(in_top_left(), in_top_left()),
        verbose = FALSE
      )

      delete_slide_raw(ps, test_slide_id)
    }
  )

  elements <- ps$get_elements(element_type = "text")
  expect_length(elements, 2L)
})
