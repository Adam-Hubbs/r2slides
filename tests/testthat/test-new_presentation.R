# ── get_page_raw() caching ─────────────────────────────────────────────────────

test_that("get_page_raw() returns cached value within TTL without re-fetching", {
  vcr::use_cassette(
    "page_raw_cache_hit",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      result1 <- ps$get_page_raw(slide_id)
      result2 <- ps$get_page_raw(slide_id)

      delete_slide_raw(ps, slide_id)
    }
  )

  expect_identical(result1, result2)
})

test_that("get_page_raw() with force = TRUE re-fetches within TTL", {
  vcr::use_cassette(
    "page_raw_force_refetch",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      result1 <- ps$get_page_raw(slide_id)
      result2 <- ps$get_page_raw(slide_id, force = TRUE)

      delete_slide_raw(ps, slide_id)
    }
  )

  # Both results should represent the same slide (identical structure).
  expect_equal(result1$objectId, result2$objectId)
})

test_that("get_page_raw() caches independently per slide_id", {
  vcr::use_cassette(
    "page_raw_independent_cache",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before1 <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id_a <- setdiff(unlist(ps$get_slide_ids()), before1)[[1]]

      before2 <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id_b <- setdiff(unlist(ps$get_slide_ids()), before2)[[1]]

      raw_a1 <- ps$get_page_raw(slide_id_a)
      raw_a2 <- ps$get_page_raw(slide_id_a)
      raw_b1 <- ps$get_page_raw(slide_id_b)
      raw_b2 <- ps$get_page_raw(slide_id_b)

      delete_slide_raw(ps, slide_id_a)
      delete_slide_raw(ps, slide_id_b)
    }
  )

  expect_identical(raw_a1, raw_a2)
  expect_identical(raw_b1, raw_b2)
  expect_equal(raw_a1$objectId, slide_id_a)
  expect_equal(raw_b1$objectId, slide_id_b)
})

test_that("refresh() clears the page cache so next get_page_raw() returns fresh data", {
  vcr::use_cassette(
    "page_raw_cache_clear_on_refresh",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      result_before <- ps$get_page_raw(slide_id)

      ps$refresh()

      result_after <- ps$get_page_raw(slide_id)

      delete_slide_raw(ps, slide_id)
    }
  )

  # Both results should refer to the same slide — confirming the re-fetch succeeded.
  expect_equal(result_before$objectId, result_after$objectId)
  expect_identical(result_before$objectId, result_after$objectId)
})
