
test_that("correct_slide_size handles default PowerPoint to Google Slides conversion", {
  expect_equal(correct_slide_size(10, "width"),
               10 * (10 / 13.33),
               label = "Default width conversion")

  expect_equal(correct_slide_size(7.5, "height"),
               7.5 * (5.625 / 7.5),
               label = "Default height conversion")
})

test_that("correct_slide_size handles custom slide size conversions", {
  # Custom slide size conversion
  custom_sizes <- list(
    x_width = 10,
    x_height = 5.6,
    y_width = 9,
    y_height = 5
  )

  expect_equal(correct_slide_size(10, "width", slide_size = custom_sizes),
               10 * (9 / 10),
               label = "Custom width conversion")

  expect_equal(correct_slide_size(5.6, "height", slide_size = custom_sizes),
               5.6 * (5 / 5.6),
               label = "Custom height conversion")
})

test_that("correct_slide_size handles partial slide size specifications", {
  partial_sizes <- list(
    x_width = 13.33,
    y_height = 6
  )

  expect_equal(correct_slide_size(7.5, "height", slide_size = partial_sizes),
               7.5 * (6 / 7.5),
               label = "Partial height conversion")

  expect_equal(correct_slide_size(10, "width", slide_size = partial_sizes),
               10 * (10 / 13.33),
               label = "Partial width conversion with defaults")
})

test_that("correct_slide_size throws errors for invalid inputs", {

  expect_error(correct_slide_size(10, "width", slide_size = "not a list"),
               "slide_size must be a list",
               label = "Non-list slide_size triggers error")


  expect_error(correct_slide_size(10, "length"),
               "must be one of",
               label = "Invalid dimension triggers error")
})

test_that("correct_slide_size handles various numeric inputs", {

  expect_equal(correct_slide_size(5.5, "width"),
               5.5 * (10 / 13.33),
               label = "Positive width conversion")


  expect_equal(correct_slide_size(0, "height"),
               0,
               label = "Zero height conversion")


  expect_equal(correct_slide_size(-3, "width"),
               -3 * (10 / 13.33),
               label = "Negative width conversion")
})

test_that("correct_slide_size maintains numeric precision", {
  precise_input <- 7.123456
  expect_equal(correct_slide_size(precise_input, "height"),
               precise_input * (5.625 / 7.5),
               label = "Precise height conversion")
})
