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
    c("id", "request", "time_requested", "resource_id", "user_call")
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
  expect_equal(buf$resource_id[[1]], "test123")
  expect_true(is.environment(buf$user_call[[1]]))
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
  expect_equal(buf$resource_id, c("presA", "presB"))
})

test_that("lazy strategy captures resource_id from spreadsheetId", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "sheets.spreadsheets.values.get",
    params = list(spreadsheetId = "sheet123", range = "A1"),
    base = "sheets",
    token = test_token
  )

  buf <- view_request_buffer()
  expect_equal(buf$resource_id[[1]], "sheet123")
})

test_that("query() with debug=TRUE bypasses lazy buffering", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

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

test_that("execute_requests() with batch_all=FALSE removes executed requests", {
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

  expect_equal(nrow(view_request_buffer()), 2L)
  suppressWarnings(execute_requests(batch_all = FALSE))
  expect_equal(nrow(view_request_buffer()), 0L)
})

test_that("execute_requests() with batch_all=TRUE removes batched requests", {
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

  expect_equal(nrow(view_request_buffer()), 2L)
  suppressWarnings(execute_requests(batch_all = TRUE))
  expect_equal(nrow(view_request_buffer()), 0L)
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

  # Buffer should be empty after execution (not 2 rows from re-buffering)
  expect_equal(nrow(view_request_buffer()), 0L)
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

test_that("sequential IDs increment across add() calls", {
  withr::local_envvar(r2slides_request_evaluation_strategy = "lazy")
  withr::defer(clear_request_buffer())

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "p1"),
    body = list(requests = list()),
    base = "slides",
    token = test_token
  )
  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "p2"),
    body = list(requests = list()),
    base = "slides",
    token = test_token
  )

  buf <- view_request_buffer()
  expect_true(buf$id[[2]] > buf$id[[1]])
})
