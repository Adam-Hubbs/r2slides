test_that("`str_before()` works correctly with single characters", {
  # Without boundary
  expect_equal(str_before("hello.world", "."), c(1, 5))

  # With boundary
  expect_equal(str_before("hello.world", ".", include_boundary = TRUE), c(1, 6))

  # At start of string
  expect_equal(str_before(".hello", "."), c(1, 0))
  expect_equal(str_before(".hello", ".", include_boundary = TRUE), c(1, 1))

  # First occurrence only
  expect_equal(str_before("a.b.c.d", "."), c(1, 1))
})

test_that("`str_before()` works correctly with multi-character strings", {
  expect_equal(str_before("hello--world", "--"), c(1, 5))
  expect_equal(
    str_before("hello--world", "--", include_boundary = TRUE),
    c(1, 7)
  )
})

test_that("`str_before()` handles special characters literally", {
  expect_equal(str_before("test$value", "$"), c(1, 4))
  expect_equal(str_before("hello→world", "→"), c(1, 5))
  expect_equal(str_before("hello world", " "), c(1, 5))
  expect_equal(str_before("hello\nworld", "\n"), c(1, 5))
})

test_that("`str_before()` handles errors appropriately", {
  expect_snapshot(error = TRUE, str_before("hello", "x"))
  expect_snapshot(error = TRUE, str_before("", "."))
  expect_snapshot(error = TRUE, str_before("hello", ""))
  expect_snapshot(error = TRUE, str_before(NULL, "."))
  expect_snapshot(error = TRUE, str_before("hello", NULL))
  expect_snapshot(error = TRUE, str_before(123, "."))
})

test_that("`str_after()` works correctly with single characters", {
  # Without boundary
  expect_equal(str_after("hello.world", "."), c(7, 11))

  # With boundary
  expect_equal(str_after("hello.world", ".", include_boundary = TRUE), c(6, 11))

  # At end of string
  expect_equal(str_after("hello.", "."), c(7, 6))
  expect_equal(str_after("hello.", ".", include_boundary = TRUE), c(6, 6))

  # First occurrence only
  expect_equal(str_after("a.b.c.d", "."), c(3, 7))
})

test_that("`str_after()` works correctly with multi-character strings", {
  expect_equal(str_after("hello--world", "--"), c(8, 12))
  expect_equal(
    str_after("hello--world", "--", include_boundary = TRUE),
    c(6, 12)
  )
})

test_that("`str_after()` handles special characters literally", {
  expect_equal(str_after("test$value", "$"), c(6, 10))
  expect_equal(str_after("hello→world", "→"), c(7, 11))
  expect_equal(str_after("hello\tworld", "\t"), c(7, 11))
})

test_that("`str_after()` handles errors appropriately", {
  expect_snapshot(error = TRUE, str_after("hello", "x"))
  expect_snapshot(error = TRUE, str_after("", "."))
  expect_snapshot(error = TRUE, str_after("hello", ""))
  expect_snapshot(error = TRUE, str_after(NULL, "."))
  expect_snapshot(error = TRUE, str_after("hello", NULL))
  expect_snapshot(error = TRUE, str_after(123, "."))
})

test_that("`str_before()` and `str_after()` are complementary", {
  text <- "hello.world"
  char <- "."

  before <- str_before(text, char)
  after <- str_after(text, char)

  expect_equal(before[2] + 1, after[1] - 1)
})

test_that("`str_matches()` works with literal and regex patterns", {
  # Literal pattern
  result <- str_matches("hello.world", "ello")
  expect_equal(result, c(2, 5))

  # Regex patterns
  expect_equal(str_matches("test123", "\\d+"), c(5, 7))
  expect_equal(
    str_matches("email@example.com", "[a-z]+@[a-z]+\\.[a-z]+"),
    c(1, 17)
  )
  expect_equal(str_matches("price: $50", "\\$\\d+"), c(8, 10))
  expect_equal(str_matches("abc123def", "[0-9]+"), c(4, 6))
  expect_equal(str_matches("hello  world", "\\s+"), c(6, 7))
  expect_equal(str_matches("hello world", "\\bworld"), c(7, 11))

  # Anchors
  expect_equal(str_matches("hello world", "^hello"), c(1, 5))
  expect_equal(str_matches("hello world", "world$"), c(7, 11))

  # First match only
  expect_equal(str_matches("abc abc abc", "abc"), c(1, 3))
})

test_that("`str_matches()` handles special cases", {
  # Empty pattern
  expect_equal(str_matches("hello", ""), c(1, 1))

  # Unicode
  expect_equal(str_matches("café", "é"), c(4, 4))
})

test_that("`str_matches()` handles errors appropriately", {
  expect_snapshot(error = TRUE, str_matches("hello", "xyz"))
  expect_snapshot(error = TRUE, str_matches("", "test"))
  expect_snapshot(error = TRUE, str_matches(NULL, "test"))
  expect_snapshot(error = TRUE, str_matches("hello", NULL))
  expect_snapshot(error = TRUE, str_matches("hello", "["))
})