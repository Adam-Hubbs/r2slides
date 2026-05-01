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
