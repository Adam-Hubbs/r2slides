test_that("query errors on unrecognized endpoints", {
  expect_snapshot(error = TRUE,
    query(endpoint = "nonexistent.endpoint", token = test_token)
  )
})

test_that("query errors on missing required parameters", {
  # Skip if mthds_slides is not available or doesn't have the endpoint
  skip_if_not(
    exists("mthds_slides") && !is.null(mthds_slides[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  # Test with missing presentationId
  expect_snapshot(error= TRUE,
    query(
      endpoint = "slides.presentations.batchUpdate",
      token = test_token,
      body = list(requests = list())
    ))
})


test_that("query errors when passed a base that conflicts with the endpoint", {
  skip_if_not(
    exists("mthds_slides") && !is.null(mthds_slides[["slides.presentations.batchUpdate"]]),
    "batchUpdate endpoint definition not available"
  )

  # Test sheets base with slides endpoint
  expect_snapshot(error = TRUE,
    query(
      endpoint = "slides.presentations.batchUpdate",
      params = list(presentationId = "test123"),
      body = list(requests = list()),
      base = "sheets",
      token = test_token
    ))
})


test_that("query errors on wrong parameters", {
  skip_if_not(
    exists("mthds_slides") && !is.null(mthds_slides[["slides.presentations.batchUpdate"]]),
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
    wrong_param = "This test parameter is not supported by this endpoint"
  )

  expect_snapshot(error = TRUE,
    query(
      endpoint = "slides.presentations.batchUpdate",
      params = test_params,
      body = test_body,
      base = "slides",
      token = test_token
    )
  )
})
