test_that("sht_id() constructs with valid args", {
  x <- sht_id(spreadsheet_id = "abc123", sheet_id = "456")
  expect_equal(x@spreadsheet_id, "abc123")
  expect_equal(x@sheet_id, "456")
})

test_that("is_sht_id() returns TRUE for sht_id and chart_id objects", {
  s <- sht_id(spreadsheet_id = "abc", sheet_id = "1")
  c_id <- chart_id(spreadsheet_id = "abc", sheet_id = "1", chart_id = "42")
  expect_true(is_sht_id(s))
  expect_true(is_sht_id(c_id))
  expect_false(is_sht_id(list()))
  expect_false(is_sht_id("character"))
})

test_that("chart_id() constructs with valid args", {
  x <- chart_id(spreadsheet_id = "abc", sheet_id = "1", chart_id = "42")
  expect_equal(x@spreadsheet_id, "abc")
  expect_equal(x@sheet_id, "1")
  expect_equal(x@chart_id, "42")
})

test_that("chart_id inherits from sht_id", {
  x <- chart_id(spreadsheet_id = "abc", sheet_id = "1", chart_id = "1")
  expect_true(S7::S7_inherits(x, sht_id))
  expect_true(S7::S7_inherits(x, chart_id))
})

test_that("sht_id print method snapshot", {
  x <- sht_id(spreadsheet_id = "abc123", sheet_id = "456")
  expect_snapshot(print(x))
})

test_that("chart_id print method snapshot", {
  x <- chart_id(spreadsheet_id = "abc123", sheet_id = "456", chart_id = "789")
  expect_snapshot(print(x))
})

test_that("sht_id() errors on non-scalar spreadsheet_id", {
  expect_snapshot(
    error = TRUE,
    sht_id(spreadsheet_id = c("a", "b"), sheet_id = "1")
  )
})

test_that("chart_id() errors on non-scalar chart_id", {
  expect_snapshot(
    error = TRUE,
    chart_id(spreadsheet_id = "abc", sheet_id = "1", chart_id = c("1", "2"))
  )
})
