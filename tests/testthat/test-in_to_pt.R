test_that("in_to_pt correctly converts inches to points", {
  expect_equal(in_to_pt(1), 72, label = "1 inch converts to 72 points")
  expect_equal(in_to_pt(0), 0, label = "0 inches converts to 0 points")
  expect_equal(in_to_pt(2.5), 180, label = "2.5 inches converts to 180 points")
  expect_equal(in_to_pt(-1), -72, label = "Negative inches convert correctly")
})

test_that("in_to_pt handles numeric vectors", {
  expect_equal(in_to_pt(c(1, 2, 3)), c(72, 144, 216),
               label = "Vector input conversion")
  expect_equal(in_to_pt(c(-1, 0, 1)), c(-72, 0, 72),
               label = "Mixed vector input")
})

test_that("in_to_pt handles floating point precision", {
  expect_equal(in_to_pt(0.5), 36, label = "Half inch converts to 36 points")
  expect_equal(in_to_pt(1/3), 72/3, label = "Fractional inch conversion")
})

test_that("in_to_pt throws error for non-numeric inputs", {
  expect_error(in_to_pt("1"),
               "Position or size argument must be numeric",
               label = "String input triggers error")

  expect_error(in_to_pt(NA),
               "Position or size argument must be numeric",
               label = "NA input triggers error")

  expect_error(in_to_pt(NULL),
               "Position or size argument must be numeric",
               label = "NULL input triggers error")

  expect_error(in_to_pt(c(1, "2")),
               "Position or size argument must be numeric",
               label = "Mixed vector with non-numeric triggers error")
})
