test_that("add_text() adds a text element and records it in the ledger", {
  vcr::use_cassette(
    "add_text_basic",
    match_requests_on = c("method", "uri", "body_json"),
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
    match_requests_on = c("method", "uri", "body_json"),
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
  expect_equal(length(elements), 2L)
  expect_true(all(purrr::map_chr(elements, "slide_id") == test_slide_id))
})

test_that("add_text() errors on non-character text", {
  vcr::use_cassette(
    "add_text_errors",
    match_requests_on = c("method", "uri", "body_json"),
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
