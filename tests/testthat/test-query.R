test_that("query2 errors on unrecognized endpoints", {
  expect_error(
    query2(endpoint = "nonexistent.endpoint", token = test_token),
    "Endpoint not recognized"
  )
})


test_that("query2 handles slides.presentations.batchUpdate endpoint", {
  # Skip if mthds is not available or doesn't have the endpoint
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  test_body <- list(
    requests = list(
      list(
        createSlide = list(
          slideLayoutReference = list(
            predefinedLayout = "TITLE_AND_BODY"
          )
        )
      )
    )
  )

  req <- query2(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = test_body,
    token = test_token
  )

  expect_type(req, "list")
  expect_true(all(c("method", "url", "token", "body") %in% names(req)))
  expect_equal(req$method, "POST")
  expect_match(req$url, "presentations/test123:batchUpdate$")
  expect_equal(req$body, test_body)
})


test_that("query2 handles slides.presentations.create endpoint", {
  # Skip if mthds is not available or doesn't have the endpoint
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.create"]]),
    "create endpoint definition not available"
  )

  test_body <- list(
    title = "Test Presentation"
  )

  req <- query2(
    endpoint = "slides.presentations.create",
    body = test_body,
    token = test_token
  )

  expect_type(req, "list")
  expect_true(all(c("method", "url", "token", "body") %in% names(req)))
  expect_equal(req$method, "POST")
  expect_match(req$url, "presentations$")
  expect_equal(req$body, test_body)
})


test_that("query2 errors on missing required parameters", {
  # Skip if mthds is not available or doesn't have the endpoint
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  # Test with missing presentationId
  expect_error(
    query2(
      endpoint = "slides.presentations.batchUpdate",
      token = test_token,
      body = list(requests = list())
    ),
    "Missing or malformed argument"
  )
})


test_that("query2 correctly handles body parameter", {
  # Skip if mthds is not available or doesn't have the endpoint
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  test_body <- list(
    requests = list(
      list(
        createShape = list(
          objectId = "test_shape",
          shapeType = "RECTANGLE"
        )
      )
    )
  )

  req <- query2(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = test_body,
    token = test_token
  )

  # Verify the body is included in the request
  expect_equal(req$body, test_body)

  # Test with NULL body
  req_null_body <- query2(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = NULL,
    token = test_token
  )

  # Body is an empty list
  expect_true(length(req_null_body$body) == 0)
})

# Test base URL construction
test_that("query2 constructs correct base URL", {
  # Skip if mthds is not available or doesn't have the endpoint
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  # Test default base (slides)
  req_default <- query2(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = list(requests = list()),
    token = test_token
  )

  expect_match(req_default$url, "^https://slides\\.googleapis\\.com")

  # Test explicit slides base
  req_slides <- query2(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = "test123"),
    body = list(requests = list()),
    base = "slides",
    token = test_token
  )

  expect_match(req_slides$url, "^https://slides\\.googleapis\\.com")
})


test_that("query2 errors when passed a base that conflicts with the endpoint", {
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  # Test sheets base with slides endpoint
  expect_error(
    query2(
      endpoint = "slides.presentations.batchUpdate",
      params = list(presentationId = "test123"),
      body = list(requests = list()),
      base = "sheets",
      token = test_token
    ),
    "Incompatible endpoint"
  )
})


test_that("query2 errors on wrong parameters", {
  skip_if_not(
    exists("mthds") && !is.null(mthds[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  test_body <- list(
    requests = list(
      list(
        createShape = list(
          objectId = "test_shape",
          shapeType = "RECTANGLE"
        )
      )
    )
  )

  test_params <- list(
    presentationId = "test123",
    wrong_param = "This test paremeter is not supported by this endpoint"
  )

  expect_error(
    query2(
      endpoint = "slides.presentations.batchUpdate",
      params = test_params,
      body = test_body,
      base = "slides",
      token = test_token
    ),
    "parameters are unknown"
  )
})
