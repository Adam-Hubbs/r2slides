test_that("set_defaults handles Qualtrics style title defaults", {
  # Test default values for Qualtrics title
  defaults <- set_defaults(default_title_args, type = "title", report_style = "qualtrics")

  expect_equal(defaults$title_font_size, 32)
  expect_false(defaults$title_font_bold)
  expect_equal(defaults$title_font_family, 'BentonSans Regular')
  expect_equal(defaults$title_color, "#000000")
  expect_equal(defaults$title_bg_color, "#FFFFFF")
  expect_equal(defaults$title_left, 0.5)
  expect_equal(defaults$title_top, 0.25)
  expect_equal(defaults$title_width, 10.5)
  expect_equal(defaults$title_height, 0.75)
})

test_that("set_defaults handles Qualtrics style commentary defaults", {
  # Test default values for Qualtrics commentary
  defaults <- set_defaults(default_commentary_args, type = "commentary", report_style = "qualtrics")

  expect_equal(defaults$commentary_font_family, 'BentonSans Regular')
  expect_equal(defaults$commentary_color, "#000000")
  expect_equal(defaults$commentary_bg_color, "#FFFFFF")
  expect_equal(defaults$commentary_left, 0.5)
  expect_equal(defaults$commentary_top, 1)
  expect_equal(defaults$commentary_width, 10.5)
  expect_equal(defaults$commentary_height, 0.75)
})

test_that("set_defaults handles Qualtrics style footer defaults", {
  # Test default values for Qualtrics footer
  defaults <- set_defaults(default_footer_args, type = "footer", report_style = "qualtrics")

  expect_equal(defaults$footer_font_family, 'BentonSans Regular')
  expect_equal(defaults$footer_color, "#000000")
  expect_equal(defaults$footer_left, 0.5)
  expect_equal(defaults$footer_top, 7)
  expect_equal(defaults$footer_width, 10.5)
  expect_equal(defaults$footer_height, 0.5)
})

test_that("set_defaults handles Municipal style title defaults", {
  # Test default values for Municipal title
  defaults <- set_defaults(default_title_args, type = "title", report_style = "municipal")

  expect_equal(defaults$title_font_size, 40)
  expect_false(defaults$title_font_bold)
  expect_equal(defaults$title_font_family, 'Flama Medium')
  expect_equal(defaults$title_color, "#FFFFFF")
  expect_equal(defaults$title_bg_color, "#9EBCDB")
  expect_equal(defaults$title_left, 0)
  expect_equal(defaults$title_top, 0)
  expect_equal(defaults$title_width, 13.33)
  expect_equal(defaults$title_height, 0.83)
})

test_that("set_defaults handles Y2 style commentary defaults", {
  # Test default values for Y2 commentary
  defaults <- set_defaults(default_commentary_args, type = "commentary", report_style = "y2")

  expect_equal(defaults$commentary_font_family, 'Flama Semicondensed Basic')
  expect_equal(defaults$commentary_color, "#767171")
  expect_equal(defaults$commentary_bg_color, "#FFFFFF")
  expect_equal(defaults$commentary_left, 0.16)
  expect_equal(defaults$commentary_top, 1.1)
  expect_equal(defaults$commentary_width, 12.82)
  expect_equal(defaults$commentary_height, 0.34)
})

test_that("set_defaults preserves user-provided values", {
  # Test that user-provided values override defaults
  custom_args <- list(
    title_font_size = 50,
    title_font_family = NULL,
    title_font_bold = NULL,
    title_color = "#FF0000",
    title_bg_color = NULL,
    title_left = NULL,
    title_top = NULL,
    title_width = NULL,
    title_height = NULL
  )

  defaults <- set_defaults(custom_args, type = "title", report_style = "qualtrics")

  expect_equal(defaults$title_font_size, 50)
  expect_equal(defaults$title_color, "#FF0000")
  expect_false(defaults$title_font_bold)
  expect_equal(defaults$title_font_family, 'BentonSans Regular')
})


test_that("set_defaults throws error for invalid inputs", {
  # Test error handling for invalid type and report_style
  expect_error(
    set_defaults(list(), type = "invalid", report_style = "qualtrics"),
    "must be one of"
  )

  expect_error(
    set_defaults(list(), type = "title", report_style = "invalid"),
    "must be one of"
  )
})

test_that("set_defaults maintains original argument names", {
  # Test that only originally provided argument names are returned
  custom_args <- list(custom_title = 42)

  defaults <- set_defaults(custom_args, type = "title", report_style = "qualtrics")

  expect_named(defaults, "custom_title")
})
