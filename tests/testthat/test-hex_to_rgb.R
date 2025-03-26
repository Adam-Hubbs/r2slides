test_that("hex_to_rgb converts standard hex colors correctly", {
  expect_equal(hex_to_rgb("#FF0000"), list(red = 1, green = 0, blue = 0),
               label = "Red conversion")
  expect_equal(hex_to_rgb("00FF00"), list(red = 0, green = 1, blue = 0),
               label = "Green conversion")
  expect_equal(hex_to_rgb("#0000FF"), list(red = 0, green = 0, blue = 1),
               label = "Blue conversion")
  expect_equal(hex_to_rgb("#FFFFFF"), list(red = 1, green = 1, blue = 1),
               label = "White conversion")
  expect_equal(hex_to_rgb("000000"), list(red = 0, green = 0, blue = 0),
               label = "Black conversion")
  expect_equal(hex_to_rgb("141414"), list(red = 0.078431373, green = 0.078431373, blue = 0.078431373),
               label = "Dark-grey conversion")
  expect_equal(hex_to_rgb("#808080"), list(red = 0.50196078, green = 0.50196078, blue = 0.50196078),
               label = "Mid-gray conversion")
})

test_that("hex_to_rgb handles mixed case hex codes", {
  expect_equal(hex_to_rgb("#aabbcc"), hex_to_rgb("#AABBCC"),
               label = "Mixed case handling")
})


test_that("hex_to_rgb throws error for invalid inputs", {
  # Test error conditions
  expect_error(hex_to_rgb("#"),
               "must be a valid 6-digit hex color code",
               label = "Empty hex code")

  expect_error(hex_to_rgb("GHIJKL"),
               "must be a valid 6-digit hex color code",
               label = "Invalid hex characters")

  expect_error(hex_to_rgb("#1234"),
               "must be a valid 6-digit hex color code",
               label = "Too short hex code")

  expect_error(hex_to_rgb("#1234567"),
               "must be a valid 6-digit hex color code",
               label = "Too long hex code")
})
