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
    on_slide_with_notes("this_pattern_xyz_does_not_exist_abc123", ps = ps)
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

# ── Error path tests ──────────────────────────────────────────────────────────

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

# ── is.slide ──────────────────────────────────────────────────────────────────

test_that("is.slide() returns TRUE for slide objects and FALSE for everything else", {
  vcr::use_cassette(
    "is_slide_type_check",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      s <- on_slide_number(1, ps)
    }
  )

  expect_true(is.slide(s))
  expect_false(is.slide(list()))
  expect_false(is.slide("a character string"))
  expect_false(is.slide(NULL))
  expect_false(is.slide(1L))
})

# ── on_slide_after ────────────────────────────────────────────────────────────

test_that("on_slide_after() returns the slide at a positive offset", {
  vcr::use_cassette(
    "slide_after_positive",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      slide_ids <- unlist(ps$get_slide_ids())
      s1 <- on_slide_number(1, ps)
      s_after <- on_slide_after(s1, offset = 1, ps)
    }
  )

  expect_true(is.slide(s_after))
  expect_equal(s_after@slide_id, slide_ids[[2]])
})

test_that("on_slide_after() navigates backwards with a negative offset", {
  vcr::use_cassette(
    "slide_after_negative",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      slide_ids <- unlist(ps$get_slide_ids())
      s3 <- on_slide_number(3, ps)
      s_prev <- on_slide_after(s3, offset = -1, ps)
    }
  )

  expect_true(is.slide(s_prev))
  expect_equal(s_prev@slide_id, slide_ids[[2]])
})

# ── slide equality ────────────────────────────────────────────────────────────

test_that("slide == returns TRUE for structurally identical slides", {
  vcr::use_cassette(
    "slide_equality_identical",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      # Slides 2 and 3 in the test presentation are structurally identical
      s2 <- on_slide_number(2, ps)
      s3 <- on_slide_number(3, ps)
      eq <- s2 == s3
    }
  )

  expect_true(eq)
})

test_that("slide == returns FALSE for structurally different slides", {
  vcr::use_cassette(
    "slide_equality_different",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      s1 <- on_slide_number(1, ps)
      s2 <- on_slide_number(2, ps)
      eq <- s1 == s2
    }
  )

  expect_false(eq)
})

# ── on_slide_with_notes extended coverage ────────────────────────────────────

test_that("on_slide_with_notes() errors on invalid text types", {
  vcr::use_cassette(
    "slide_with_notes_errors",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  expect_snapshot(error = TRUE, on_slide_with_notes(123, ps = ps))
  expect_snapshot(error = TRUE, on_slide_with_notes(NA_character_, ps = ps))
  expect_snapshot(error = TRUE, on_slide_with_notes(c("a", "b"), ps = ps))
})

test_that("on_slide_with_notes() finds an exact single-match note", {
  vcr::use_cassette(
    "slide_with_notes_exact_single",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      # Slide 5 has notes "Notes" — no other slide matches exactly
      result <- on_slide_with_notes("Notes", ps = ps)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3db51d43376_0_17")
})

test_that("on_slide_with_notes() finds a slide using regex matching", {
  vcr::use_cassette(
    "slide_with_notes_regex",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      # Slide 4 is the only slide whose notes contain "hopefully"
      result <- on_slide_with_notes("hopefully", match = "regex", ps = ps)
    }
  )

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3db51d43376_0_12")
})

test_that("on_slide_with_notes() errors by default when multiple slides match", {
  vcr::use_cassette(
    "slide_with_notes_multi_match_error",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  # "^Notes" matches both slide 4 ("Notes hopefully they match") and slide 5 ("Notes")
  expect_snapshot(
    error = TRUE,
    on_slide_with_notes("^Notes", match = "regex", ps = ps)
  )
})

test_that("on_slide_with_notes() returns all matches when on_multiple = 'return'", {
  vcr::use_cassette(
    "slide_with_notes_return_multi",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      result <- on_slide_with_notes(
        "^Notes",
        match = "regex",
        on_multiple = "return",
        ps = ps
      )
    }
  )

  expect_type(result, "list")
  expect_equal(length(result), 2L)
  expect_true(all(purrr::map_lgl(result, is.slide)))
  # Names are the slide IDs
  expect_in(
    names(result),
    c("g3db51d43376_0_12", "g3db51d43376_0_17")
  )
})
