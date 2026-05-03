test_that("get_evaluation_strategy() returns 'eager' by default", {
  withr::local_envvar(r2slides_request_evaluation_strategy = NA_character_)
  expect_equal(get_evaluation_strategy(), "eager")
})

test_that("set_evaluation_strategy() sets the strategy", {
  withr::local_envvar(r2slides_request_evaluation_strategy = NA_character_)
  set_evaluation_strategy("lazy")
  expect_equal(get_evaluation_strategy(), "lazy")
  set_evaluation_strategy("eager")
  expect_equal(get_evaluation_strategy(), "eager")
})

test_that("set_evaluation_strategy() rejects invalid strategies", {
  expect_snapshot(error = TRUE, set_evaluation_strategy("invalid"))
})

test_that("get_evaluation_strategy() warns on unknown env var value", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "turbo")
  expect_snapshot(get_evaluation_strategy())
})

test_that("view_request_buffer() returns a tibble with the right columns", {
  buf <- view_request_buffer()
  expect_s3_class(buf, "tbl_df")
  expect_named(
    buf,
    c(
      "request",
      "time_requested",
      "tried_to_execute",
      "execute_succeeded",
      "presentation"
    )
  )
})

test_that("lazy strategy buffers query() calls without executing", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  result <- query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = list(requests = list()),
    base = "slides",
    token = test_token
  )

  expect_null(result)
  buf <- view_request_buffer()
  expect_equal(nrow(buf), 1L)
  expect_equal(buf$presentation[[1]], "test123")
  expect_false(buf$tried_to_execute[[1]])
  expect_identical(buf$execute_succeeded[[1]], NA)
})

test_that("lazy strategy buffers multiple calls independently", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "presA"),
    body = list(requests = list(list(a = 1))),
    base = "slides",
    token = test_token
  )
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "presB"),
    body = list(requests = list(list(b = 2))),
    base = "slides",
    token = test_token
  )

  buf <- view_request_buffer()
  expect_equal(nrow(buf), 2L)
  expect_equal(buf$presentation, c("presA", "presB"))
})

test_that("query() with debug=TRUE bypasses lazy buffering", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  # debug=TRUE should return a request object, not buffer it
  result <- query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = list(requests = list()),
    base = "slides",
    token = test_token,
    debug = TRUE
  )

  expect_false(is.null(result))
  expect_equal(nrow(view_request_buffer()), 0L)
})

test_that("execute_requests() with batch_all=FALSE marks each request as tried", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test_pres"),
    body = list(requests = list(list(createShape = list()))),
    base = "slides",
    token = test_token
  )
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test_pres"),
    body = list(requests = list(list(insertText = list()))),
    base = "slides",
    token = test_token
  )

  # Execution will fail (no real token), but the buffer should be updated
  suppressWarnings(execute_requests(batch_all = FALSE))

  buf <- view_request_buffer()
  expect_true(all(buf$tried_to_execute))
  expect_true(all(!buf$execute_succeeded))
})

test_that("execute_requests() with batch_all=TRUE marks all same-presentation requests as tried", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "pres1"),
    body = list(requests = list(list(createShape = list()))),
    base = "slides",
    token = test_token
  )
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "pres1"),
    body = list(requests = list(list(insertText = list()))),
    base = "slides",
    token = test_token
  )

  suppressWarnings(execute_requests(batch_all = TRUE))

  buf <- view_request_buffer()
  expect_true(all(buf$tried_to_execute))
  # Both rows correspond to one combined API call, so both share the same outcome
  expect_equal(buf$execute_succeeded[[1]], buf$execute_succeeded[[2]])
})

test_that("execute_requests() does not re-buffer when called in lazy mode", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test_pres"),
    body = list(requests = list()),
    base = "slides",
    token = test_token
  )

  suppressWarnings(execute_requests(batch_all = FALSE))

  # Buffer should still have exactly 1 row, not 2 (no re-buffering)
  buf <- view_request_buffer()
  expect_equal(nrow(buf), 1L)
})

test_that("execute_requests() reports no pending requests when buffer is empty", {
  withr::defer(clear_request_buffer())
  expect_snapshot(execute_requests())
})

test_that("clear_request_buffer() empties the buffer", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = list(requests = list()),
    base = "slides",
    token = test_token
  )

  expect_equal(nrow(view_request_buffer()), 1L)
  clear_request_buffer()
  expect_equal(nrow(view_request_buffer()), 0L)
})

test_that("buffered request stores correct query arguments", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  body <- list(requests = list(list(createShape = list(objectId = "abc"))))
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "pres_xyz"),
    body = body,
    base = "slides",
    token = test_token
  )

  buf <- view_request_buffer()
  req <- buf$request[[1]]
  expect_equal(req$endpoint, "slides.presentations.batchUpdate")
  expect_equal(req$params$presentationId, "pres_xyz")
  expect_equal(req$body, body)
  expect_equal(req$base, "slides")
})
