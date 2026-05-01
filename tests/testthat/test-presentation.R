# jarl-ignore internal_function: Used for testing only
test_that("register_presentation() opens by direct ID and populates fields", {
  vcr::use_cassette(
    "presentation_register_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  expect_true(is.presentation(ps))
  expect_equal(ps$presentation_id, TEST_PRESENTATION_ID)
  expect_type(ps$title, "character")
  expect_true(length(ps$get_slide_ids()) > 0)
})

test_that("register_presentation() opens by full URL", {
  url <- paste0(
    "https://docs.google.com/presentation/d/",
    TEST_PRESENTATION_ID,
    "/edit"
  )

  vcr::use_cassette(
    "presentation_register_url",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = url, set_active = FALSE)
    }
  )

  expect_true(is.presentation(ps))
  expect_equal(ps$presentation_id, TEST_PRESENTATION_ID)
})

# ── Pure unit tests (no API) ──────────────────────────────────────────────────

test_that("is.presentation() returns FALSE for non-presentation objects", {
  expect_false(is.presentation(list()))
  expect_false(is.presentation("character"))
  expect_false(is.presentation(NULL))
  expect_false(is.presentation(42L))
  expect_false(is.presentation(data.frame()))
})

test_that("get_active_presentation() errors when no active presentation", {
  old <- if (
    exists("active_presentation", envir = r2slides:::.r2slides_objects)
  ) {
    get("active_presentation", envir = r2slides:::.r2slides_objects)
  } else {
    NULL
  }
  assign("active_presentation", NULL, envir = r2slides:::.r2slides_objects)
  withr::defer(
    assign("active_presentation", old, envir = r2slides:::.r2slides_objects)
  )

  expect_snapshot(error = TRUE, get_active_presentation())
})

test_that("active_presentation_exists() returns FALSE when no active presentation is set", {
  old <- if (
    exists("active_presentation", envir = r2slides:::.r2slides_objects)
  ) {
    get("active_presentation", envir = r2slides:::.r2slides_objects)
  } else {
    NULL
  }
  assign("active_presentation", NULL, envir = r2slides:::.r2slides_objects)
  withr::defer(
    assign("active_presentation", old, envir = r2slides:::.r2slides_objects)
  )

  expect_false(active_presentation_exists())
})

# ── API-backed presentation method tests ──────────────────────────────────────

test_that("presentation$get_url() returns a valid Google Slides URL containing the presentation ID", {
  vcr::use_cassette(
    "presentation_get_url",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  url <- ps$get_url()
  expect_type(url, "character")
  expect_true(startsWith(url, "https://docs.google.com/presentation/d/"))
  expect_match(url, TEST_PRESENTATION_ID, fixed = TRUE)
})

test_that("presentation$is_active() is FALSE when set_active = FALSE", {
  vcr::use_cassette(
    "presentation_is_active",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  expect_false(ps$is_active())
})

test_that("add_to_ledger() records elements; get_elements() filters correctly", {
  vcr::use_cassette(
    "presentation_ledger_operations",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  # Empty ledger returns NULL
  expect_null(ps$get_elements())

  # Populate with two types
  ps$add_to_ledger("e001", "slide_a", "text", "Hello")
  ps$add_to_ledger("e002", "slide_a", "table", NA_character_)
  ps$add_to_ledger("e003", "slide_b", "text", "World")

  all_elems <- ps$get_elements()
  expect_length(all_elems, 3L)

  # Filter by element_type
  text_elems <- ps$get_elements(element_type = "text")
  expect_length(text_elems, 2L)
  text_ids <- purrr::map_chr(text_elems, "element_id")
  expect_in(text_ids, c("e001", "e003"))

  table_elems <- ps$get_elements(element_type = "table")
  expect_length(table_elems, 1L)
  expect_equal(table_elems[[1]]$element_id, "e002")

  # Filter by element_text
  hello_elems <- ps$get_elements(element_text = "Hello")
  expect_length(hello_elems, 1L)
  expect_equal(hello_elems[[1]]$element_id, "e001")

  # Type with no matches returns NULL
  expect_null(ps$get_elements(element_type = "chart"))

  # slide_id is also recorded
  slide_a_elems <- purrr::keep(all_elems, \(e) e$slide_id == "slide_a")
  expect_length(slide_a_elems, 2L)
})

test_that("presentation$get_slide_notes_text() returns notes for slides with and without speaker notes", {
  vcr::use_cassette(
    "presentation_notes_text",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
    }
  )

  # Slide 4 has "Notes hopefully they match"
  notes_4 <- ps$get_slide_notes_text("g3db51d43376_0_12")
  expect_type(notes_4, "character")
  expect_equal(notes_4, "Notes hopefully they match")

  # Slide 2 has no notes (empty string)
  notes_empty <- ps$get_slide_notes_text("g3db51d43376_0_0")
  expect_equal(notes_empty, "")

  # Non-existent slide_id errors
  expect_snapshot(
    error = TRUE,
    ps$get_slide_notes_text("this_id_does_not_exist")
  )
})

test_that("presentation$get_slide_by_index() returns the correct slide and errors on bad inputs", {
  vcr::use_cassette(
    "presentation_get_slide_by_index",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      s <- ps$get_slide_by_index(1)
      first_id <- unlist(ps$get_slide_ids())[[1]]
    }
  )

  expect_true(is.slide(s))
  expect_equal(s@slide_id, first_id)

  # Out-of-bounds, zero, NULL, non-numeric
  expect_snapshot(error = TRUE, ps$get_slide_by_index(9999))
  expect_snapshot(error = TRUE, ps$get_slide_by_index(0))
  expect_snapshot(error = TRUE, ps$get_slide_by_index(NULL))
  expect_snapshot(error = TRUE, ps$get_slide_by_index("a"))
})

test_that("presentation$get_slide_by_id() returns the correct slide and errors on bad inputs", {
  vcr::use_cassette(
    "presentation_get_slide_by_id",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      first_id <- unlist(ps$get_slide_ids())[[1]]
      s <- ps$get_slide_by_id(first_id)
    }
  )

  expect_true(is.slide(s))
  expect_equal(s@slide_id, first_id)

  # Non-existent ID, NULL, vector, numeric
  expect_snapshot(error = TRUE, ps$get_slide_by_id("this_id_does_not_exist"))
  expect_snapshot(error = TRUE, ps$get_slide_by_id(NULL))
  expect_snapshot(error = TRUE, ps$get_slide_by_id(c("a", "b")))
  expect_snapshot(error = TRUE, ps$get_slide_by_id(123))
})

test_that("presentation$get_slide_index() returns the 1-based position of a slide", {
  vcr::use_cassette(
    "presentation_get_slide_index",
    match_requests_on = c("method", "uri", "body_json"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)
      s1 <- ps$get_slide_by_index(1)
      idx <- ps$get_slide_index(s1)
    }
  )

  expect_equal(idx, 1L)

  # Non-slide input errors
  expect_snapshot(error = TRUE, ps$get_slide_index("not_a_slide"))
  expect_snapshot(error = TRUE, ps$get_slide_index(NULL))
})
