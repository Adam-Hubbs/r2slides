test_that("`slide()` can be created with valid inputs", {
  slide_id <- 'g3c5ef24c0f9_0_0'

  s <- slide(presentation = ps, slide_id = slide_id)

  expect_true(is.slide(s))
  expect_equal(s@slide_id, slide_id)
  expect_equal(s@presentation, ps)
})

test_that("`slide()` requires a valid presentation", {
  invalid_pres <- list(presentation_id = "test")
  slide_id <- "g3c5ef24c0f9_0_0"

  expect_error(
    slide(presentation = invalid_pres, slide_id = slide_id),
    "presentation must be of class `presentation`"
  )
})

test_that("`slide()` requires a single slide_id", {
  expect_error(
    slide(
      presentation = ps,
      slide_id = c("g3c5ef24c0f9_0_0", "g3c5ef24c0f9_0_4")
    ),
    "slide_id must be a single value"
  )

  expect_error(
    slide(presentation = ps, slide_id = character(0)),
    "slide_id must be a single value"
  )
})

test_that("`slide()` validates slide_id is character type", {
  expect_snapshot(error = TRUE,
    slide(presentation = ps, slide_id = 123)
  )

  expect_snapshot(error = TRUE,
    slide(presentation = ps, slide_id = NULL)
  )
})


test_that("slide@slide_hash` works", {
  local_mocked_bindings(query = function(endpoint, params, base) {
    ps$.__enclos_env__$private$slides[[1]]
  })

  s1 <- slide(presentation = ps, slide_id = "p")
  s1_copy <- slide(presentation = ps, slide_id = "p")

  hash1 <- s1@slide_hash
  hash1_copy <- s1_copy@slide_hash

  local_mocked_bindings(query = function(endpoint, params, base) {
    ps$.__enclos_env__$private$slides[[2]]
  })

  s2 <- slide(presentation = ps, slide_id = "g3c5ef24c0f9_0_0")

  hash2 <- s2@slide_hash

  local_mocked_bindings(query = function(endpoint, params, base) {
    ps$.__enclos_env__$private$slides[[3]]
  })


  s3 <- slide(presentation = ps, slide_id = "g3c5ef24c0f9_0_4")

  hash3 <- s3@slide_hash
  expect_type(hash1, "character")
  expect_equal(hash1, hash1_copy)
  expect_equal(hash1, hash2)
  expect_false(hash2 == hash3)
})


test_that("`on_slide_id()` works with valid slide ID", {
  slide_id <- "g3c5ef24c0f9_0_0"

  result <- on_slide_id(slide_id, ps)

  expect_true(is.slide(result))
  expect_equal(result@slide_id, slide_id)
})

test_that("`on_slide_id()` uses active presentation when ps missing", {
  slide_id <- "g3c5ef24c0f9_0_0"

  result <- on_slide_id(slide_id)

  expect_true(is.slide(result))
})

test_that("`on_slide_id()` fails with NULL id", {
  expect_snapshot(error = TRUE,
    on_slide_id(NULL, ps)
  )
})

test_that("`on_slide_id()` fails with NA id", {
  expect_snapshot(error = TRUE,
    on_slide_id(NA, ps)
  )
})

test_that("`on_slide_id()` fails with multiple ids", {
  expect_snapshot(error = TRUE,
    on_slide_id(c("g3c5ef24c0f9_0_0", "g3c5ef24c0f9_0_4"), ps)
  )
})

test_that("`on_slide_url()` extracts slide from valid URL", {
  pres_id <- ps$presentation_id
  slide_id <- "g3c5ef24c0f9_0_0"
  url <- paste0(
    "https://docs.google.com/presentation/d/",
    pres_id,
    "/edit#slide=id.",
    slide_id
  )

  local_mocked_bindings(
    resolve_slide_id = function(id, presentation_id) slide_id
  )

  result <- on_slide_url(url, ps)

  expect_true(is.slide(result))
})

test_that("`on_slide_url()` resolves presentation from URL when ps missing", {
  pres_id <- ps$presentation_id
  slide_id <- "g3c5ef24c0f9_0_0"
  url <- paste0(
    "https://docs.google.com/presentation/d/",
    pres_id,
    "/edit#slide=id.",
    slide_id
  )

  local_mocked_bindings(
    resolve_presentation_id = function(id) pres_id,
    resolve_slide_id = function(id, presentation_id) slide_id
  )

  expect_error(on_slide_url(url))
})

test_that("`on_slide_number()` works with valid numeric index", {
  n <- 2

  result <- on_slide_number(n, ps)

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3c5ef24c0f9_0_0")
})

test_that("`on_slide_number()` fails with NULL", {
  expect_error(
    on_slide_number(NULL, ps),
    "must be a single numeric value"
  )
})

test_that("`on_slide_number()` fails with NA", {
  expect_error(
    on_slide_number(NA, ps),
    "must be a single numeric value"
  )
})

test_that("`on_slide_number()` fails with non-numeric input", {
  expect_error(
    on_slide_number("not_a_number", ps),
    "must be a single numeric value"
  )
})

test_that("`on_slide_number()` fails with multiple values", {
  expect_error(
    on_slide_number(c(1, 2), ps),
    "must be a single numeric value"
  )
})

test_that("`on_slide_number()` uses active presentation when ps missing", {
  result <- on_slide_number(1)

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "p")
})

test_that("`on_slide_after()` returns slide after reference", {
  ref_slide <- ps$get_slide_by_index(1)

  result <- on_slide_after(ref_slide, offset = 1, ps)

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3c5ef24c0f9_0_0")
})

test_that("`on_slide_after()` works with negative offset", {
  ref_slide <- ps$get_slide_by_index(3)

  result <- on_slide_after(ref_slide, offset = -1, ps)

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3c5ef24c0f9_0_0")
})


test_that("`on_slide_after()` uses active presentation when ps missing", {
  ref_slide <- ps$get_slide_by_index(1)

  result <- on_slide_after(ref_slide, offset = 1)

  expect_true(is.slide(result))
  expect_equal(result@slide_id, "g3c5ef24c0f9_0_0")
})

test_that("`resolve_presentation_id()` extracts ID from URL", {
  pres_id <- ps$presentation_id
  url <- paste0("https://docs.google.com/presentation/d/", pres_id, "/edit")

  local_mocked_bindings(
    query = function(endpoint, params, base) list(presentationId = pres_id)
  )

  result <- resolve_presentation_id(url)

  expect_equal(result, pres_id)
})

test_that("`resolve_presentation_id()` validates direct ID format", {
  pres_id <- ps$presentation_id

  local_mocked_bindings(
    query = function(endpoint, params, base) list(presentationId = pres_id)
  )

  result <- resolve_presentation_id(pres_id)

  expect_equal(result, pres_id)
})

test_that("`resolve_presentation_id()` fails with non-character input", {
  expect_error(
    resolve_presentation_id(123),
    "must be a single string"
  )
})

test_that("`resolve_presentation_id()` fails with multiple values", {
  expect_error(
    resolve_presentation_id(c("id1", "id2")),
    "must be a single string"
  )
})



test_that("`on_slide_after()` handles edge case at beginning of presentation", {
  first_slide <- ps$get_slide_by_index(1)

  expect_error(
    on_slide_after(first_slide, offset = -1, ps)
  )
})

test_that("`on_slide_after()` handles edge case at end of presentation", {
  last_slide <- ps$get_slide_by_index(4)

  expect_error(
    on_slide_after(last_slide, offset = 5, ps)
  )
})

test_that("`resolve_presentation_id()` handles URL with extra parameters", {
  pres_id <- "abc123def456"
  url <- paste0(
    "https://docs.google.com/presentation/d/",
    pres_id,
    "/edit?usp=sharing&slide=id.xyz"
  )

  local_mocked_bindings(
    query = function(endpoint, params, base) list(presentationId = pres_id)
  )

  result <- resolve_presentation_id(url)

  expect_equal(result, pres_id)
})
