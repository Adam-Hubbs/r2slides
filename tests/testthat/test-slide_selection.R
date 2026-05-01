test_that("on_slide_number() returns a slide at the given index", {
  vcr::use_cassette(
    "slide_on_number",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      s <- on_slide_number(1, ps)
    }
  )

  expect_true(is.slide(s))
  expect_type(s@slide_id, "character")
  expect_true(nchar(s@slide_id) > 0)
})

test_that("on_slide_id() returns the slide with the given ID", {
  vcr::use_cassette(
    "slide_on_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      first_id <- unlist(ps$get_slide_ids())[[1]]
      s <- on_slide_id(first_id, ps)
    }
  )

  expect_true(is.slide(s))
  expect_equal(s@slide_id, first_id)
})

test_that("on_slide_url() resolves a slide from its full URL", {
  vcr::use_cassette(
    "slide_on_url",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      first_id <- unlist(ps$get_slide_ids())[[1]]
      url <- paste0(
        "https://docs.google.com/presentation/d/",
        ps$presentation_id,
        "/edit#slide=id.",
        first_id
      )
      s <- on_slide_url(url, ps)
    }
  )

  expect_true(is.slide(s))
  expect_equal(s@slide_id, first_id)
})

test_that("on_slide_with_notes() errors when no slide matches", {
  vcr::use_cassette(
    "slide_with_notes_no_match",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  expect_snapshot(
    error = TRUE,
    on_slide_with_notes("this_pattern_xyz_does_not_exist_abc123", ps)
  )
})

test_that("on_slide_with_notes() finds a slide with matching notes", {
  vcr::use_cassette(
    "slide_with_notes_match",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      # Slide 4 in the test presentation has notes "Notes hopefully they match"
      result <- on_slide_with_notes("Notes hopefully they match", ps = ps)
      expected_id <- "g3db51d43376_0_12"
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, expected_id)
})

# ── Error path tests (no cassette needed) ──────────────────────────────────────

test_that("on_slide_number() errors with invalid input", {
  vcr::use_cassette(
    "slide_on_number_errors",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  expect_snapshot(error = TRUE, on_slide_number(NULL, ps))
  expect_snapshot(error = TRUE, on_slide_number(NA, ps))
  expect_snapshot(error = TRUE, on_slide_number(c(1, 2), ps))
  expect_snapshot(error = TRUE, on_slide_number("a", ps))
})

test_that("on_slide_id() errors with invalid input", {
  vcr::use_cassette(
    "slide_on_id_errors",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  expect_snapshot(error = TRUE, on_slide_id(NULL, ps))
  expect_snapshot(error = TRUE, on_slide_id(NA, ps))
  expect_snapshot(error = TRUE, on_slide_id(c("a", "b"), ps))
})
