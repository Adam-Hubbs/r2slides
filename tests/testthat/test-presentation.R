# ===========================================================================
# 1. Constructor — new_presentation()
# ===========================================================================

test_that("new_presentation() populates all fields correctly", {
  vcr::use_cassette("pres_create_new_fields", {
    pres <- new_presentation(title = "My New Deck", set_active = FALSE)
  })

  expect_true(is.presentation(pres))
  expect_equal(pres$title, "My New Deck")
  expect_false(is.null(pres$presentation_id))
  expect_false(is.null(pres$revision_id))
  expect_false(is.null(pres$last_refreshed))
  expect_false(pres$is_active())
})

test_that("new_presentation() uses default title when none supplied", {
  vcr::use_cassette("pres_create_default_title", {
    pres <- new_presentation(set_active = FALSE)
  })

  expect_equal(pres$title, "Untitled Presentation")
})

test_that("new_presentation() sets and registers the active presentation", {
  withr::defer({
    if (active_presentation_exists()) get_active_presentation()$set_not_active()
  })

  vcr::use_cassette("pres_create_set_active", {
    pres <- new_presentation(title = "Active Deck", set_active = TRUE)
  })

  expect_true(pres$is_active())
  expect_true(active_presentation_exists())
  expect_true(is.presentation(get_active_presentation()))
})


# ===========================================================================
# 2. Constructor — register_presentation()
# ===========================================================================

test_that("register_presentation() opens an existing presentation by ID", {
  vcr::use_cassette("pres_open_basic", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_true(is.presentation(pres))
  expect_false(is.null(pres$presentation_id))
  expect_false(is.null(pres$title))
})


# ===========================================================================
# 3. Active presentation management
# ===========================================================================

test_that("set_active() / set_not_active() / is_active() round-trip correctly", {
  withr::defer({
    if (active_presentation_exists()) get_active_presentation()$set_not_active()
  })

  vcr::use_cassette("pres_active_roundtrip", {
    pres <- new_presentation(title = "Roundtrip", set_active = FALSE)
  })

  expect_false(pres$is_active())

  pres$set_active()
  expect_true(pres$is_active())
  expect_true(active_presentation_exists())

  pres$set_not_active()
  expect_false(pres$is_active())
  expect_false(active_presentation_exists())
})

test_that("activating a second presentation deactivates the first", {
  withr::defer({
    if (active_presentation_exists()) get_active_presentation()$set_not_active()
  })

  vcr::use_cassette("pres_active_swap", {
    pres1 <- new_presentation(title = "First", set_active = TRUE)
    pres2 <- new_presentation(title = "Second", set_active = FALSE)
  })

  pres2$set_active()

  expect_false(pres1$is_active())
  expect_true(pres2$is_active())
  expect_equal(get_active_presentation()$presentation_id, pres2$presentation_id)
})

test_that("get_active_presentation() errors when none is registered", {
  if (active_presentation_exists()) {
    get_active_presentation()$set_not_active()
  }

  expect_snapshot(get_active_presentation(), error = TRUE)
})

test_that("active_presentation_exists() returns FALSE when none registered", {
  if (active_presentation_exists()) {
    get_active_presentation()$set_not_active()
  }

  expect_false(active_presentation_exists())
})


# ===========================================================================
# 4. refresh()
# ===========================================================================

test_that("refresh() fetches updated data and advances last_refreshed", {
  vcr::use_cassette("pres_refresh", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
    t_before <- pres$last_refreshed
    Sys.sleep(0.01)
    pres$refresh()
  })

  expect_gt(as.numeric(pres$last_refreshed), as.numeric(t_before))
  expect_false(is.null(pres$title))
})

test_that("refresh() errors when presentation_id is NULL", {
  vcr::use_cassette("pres_refresh_null_id", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })
  pres$presentation_id <- NULL

  expect_snapshot(pres$refresh(), error = TRUE)
})


# ===========================================================================
# 5. Slide access
# ===========================================================================

test_that("get_slide_ids() returns a non-empty list after opening", {
  vcr::use_cassette("pres_slide_ids", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  ids <- pres$get_slide_ids()
  expect_type(ids, "list")
  expect_gt(length(ids), 0L)
})

test_that("get_slide_by_index() returns a slide object for a valid index", {
  vcr::use_cassette("pres_get_slide_index_valid", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_true(is.slide(pres$get_slide_by_index(1)))
})

test_that("get_slide_by_index() errors on out-of-bounds index", {
  vcr::use_cassette("pres_get_slide_index_oob", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })
  n <- length(pres$get_slide_ids())

  expect_snapshot(pres$get_slide_by_index(n + 1L), error = TRUE)
})

test_that("get_slide_by_index() errors on a string input", {
  vcr::use_cassette("pres_get_slide_index_bad_string", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(pres$get_slide_by_index("one"), error = TRUE)
})

test_that("get_slide_by_index() errors on a vector input", {
  vcr::use_cassette("pres_get_slide_index_bad_vector", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(pres$get_slide_by_index(c(1L, 2L)), error = TRUE)
})

test_that("get_slide_by_index() errors on NA input", {
  vcr::use_cassette("pres_get_slide_index_bad_na", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(pres$get_slide_by_index(NA_integer_), error = TRUE)
})


test_that("get_slide_by_id() returns a slide for a valid ID", {
  vcr::use_cassette("pres_get_slide_by_id_valid", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  valid_id <- pres$get_slide_ids()[[1]]
  expect_true(is.slide(pres$get_slide_by_id(valid_id)))
})

test_that("get_slide_by_id() errors on an unknown ID", {
  vcr::use_cassette("pres_get_slide_by_id_unknown", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(pres$get_slide_by_id("nonexistent_id"), error = TRUE)
})

test_that("get_slide_by_id() errors on integer input", {
  vcr::use_cassette("pres_get_slide_by_id_bad_int", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(pres$get_slide_by_id(123L), error = TRUE)
})

test_that("get_slide_by_id() errors on a character vector input", {
  vcr::use_cassette("pres_get_slide_by_id_bad_vector", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(pres$get_slide_by_id(c("a", "b")), error = TRUE)
})


# ===========================================================================
# 6. Ledger
# ===========================================================================

test_that("add_to_ledger() appends an entry retrievable via get_elements()", {
  vcr::use_cassette("pres_ledger_add_single", {
    pres <- new_presentation(title = "Ledger Test", set_active = FALSE)
  })

  pres$add_to_ledger(
    element_id = "elem_001",
    slide_id = "slide_id_1",
    element_type = "textBox",
    element_text = "Hello world"
  )

  elems <- pres$get_elements()
  expect_true(any(vapply(elems, \(e) e$element_id == "elem_001", logical(1))))
})

test_that("add_to_ledger() accumulates multiple entries", {
  vcr::use_cassette("pres_ledger_add_multi", {
    pres <- new_presentation(title = "Multi Ledger", set_active = FALSE)
  })

  for (i in seq_len(3)) {
    pres$add_to_ledger(
      element_id = paste0("elem_00", i),
      slide_id = "slide_id_1",
      element_type = "textBox",
      element_text = paste("Text", i)
    )
  }

  expect_length(pres$get_elements(), 3L)
})


# ===========================================================================
# 7. get_url() / browse() / print() / is.presentation()
# ===========================================================================

test_that("get_url() returns a well-formed Google Slides URL", {
  vcr::use_cassette("pres_get_url", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  url <- pres$get_url()
  expect_match(url, "^https://docs\\.google\\.com/presentation/d/")
  expect_match(url, pres$presentation_id, fixed = TRUE)
})

test_that("get_url() returns NULL when presentation_id is NULL", {
  vcr::use_cassette("pres_get_url_null_id", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })
  pres$presentation_id <- NULL

  expect_null(pres$get_url())
})

test_that("browse() errors when presentation_id is NULL", {
  vcr::use_cassette("pres_browse_null_id", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })
  pres$presentation_id <- NULL

  expect_snapshot(pres$browse(), error = TRUE)
})

test_that("browse() calls browseURL and returns self invisibly", {
  vcr::use_cassette("pres_browse", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  local_mocked_bindings(
    browseURL = function(...) invisible(NULL),
    .package = "utils"
  )
  expect_invisible(pres$browse())
})

test_that("print() produces expected output", {
  vcr::use_cassette("pres_print", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })

  expect_snapshot(print(pres), transform = scrub_last_refreshed)
})

test_that("is.presentation() correctly identifies the class", {
  vcr::use_cassette("pres_is_presentation", {
    pres <- new_presentation(title = "Class Check", set_active = FALSE)
  })

  expect_true(is.presentation(pres))
  expect_false(is.presentation(list()))
  expect_false(is.presentation(NULL))
  expect_false(is.presentation("a string"))
  expect_false(is.presentation(42L))
})


# ===========================================================================
# 8. copy() and delete()
# ===========================================================================

test_that("copy() returns a new presentation with a distinct ID", {
  vcr::use_cassette("pres_copy_named", {
    original <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
    copied <- original$copy(name = "Copy of Test")
  })

  expect_true(is.presentation(copied))
  expect_false(identical(original$presentation_id, copied$presentation_id))
  expect_false(copied$is_active())
})

test_that("copy() prepends 'Copy of' to the original title when no name supplied", {
  vcr::use_cassette("pres_copy_default_name", {
    original <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
    copied <- original$copy()
  })

  expect_equal(copied$title, paste("Copy of", original$title))
})

test_that("copy() errors when presentation_id is NULL", {
  vcr::use_cassette("pres_copy_null_id", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })
  pres$presentation_id <- NULL

  expect_snapshot(pres$copy(), error = TRUE)
})

test_that("delete() trashes the presentation and returns NULL", {
  vcr::use_cassette("pres_delete", {
    pres <- new_presentation(title = "To Be Deleted", set_active = FALSE)
    result <- pres$delete()
  })

  expect_null(result)
})

test_that("delete() errors when presentation_id is NULL", {
  vcr::use_cassette("pres_delete_null_id", {
    pres <- register_presentation(id = "Testing Pres 5", set_active = FALSE)
  })
  pres$presentation_id <- NULL

  expect_snapshot(pres$delete(), error = TRUE)
})