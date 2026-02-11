test_that("in_to_emu converts inches to EMU correctly", {
  # Basic conversion
  expect_equal(in_to_emu(1), 914400)
  expect_equal(in_to_emu(0), 0)
  expect_equal(in_to_emu(8), 7315200)

  # Fractional values
  expect_equal(in_to_emu(0.5), 457200)
  expect_equal(in_to_emu(2.5), 2286000)

  # Vector input
  expect_equal(in_to_emu(c(1, 2, 3)), c(914400, 1828800, 2743200))
})

test_that("in_to_emu handles negative values", {
  expect_equal(in_to_emu(-1), -914400)
  expect_equal(in_to_emu(-0.5), -457200)
})

test_that("in_to_emu rejects non-numeric input", {
  expect_error(in_to_emu("1"), "Position or size argument must be numeric")
  expect_error(in_to_emu(NULL), "Position or size argument must be numeric")
  expect_error(in_to_emu(TRUE), "Position or size argument must be numeric")
})