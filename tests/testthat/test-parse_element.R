EMU <- 914400

# position_from_raw ------------------------------------------------------------

test_that("position_from_raw() handles identity transform", {
  raw <- list(
    size = list(
      width = list(magnitude = 2 * EMU),
      height = list(magnitude = 1 * EMU)
    ),
    transform = list(
      scaleX = 1,
      scaleY = 1,
      shearX = 0,
      shearY = 0,
      translateX = 1 * EMU,
      translateY = 0.5 * EMU
    )
  )
  pos <- position_from_raw(raw)
  expect_equal(pos@left, 1)
  expect_equal(pos@top, 0.5)
  expect_equal(pos@width, 2)
  expect_equal(pos@height, 1)
  expect_equal(pos@rotation, 0)
})

test_that("position_from_raw() handles 90-degree rotation", {
  angle <- 90 * pi / 180
  raw <- list(
    size = list(
      width = list(magnitude = 2 * EMU),
      height = list(magnitude = 1 * EMU)
    ),
    transform = list(
      scaleX = cos(angle),
      scaleY = cos(angle),
      shearX = -sin(angle),
      shearY = sin(angle),
      translateX = 0,
      translateY = 0
    )
  )
  pos <- position_from_raw(raw)
  expect_equal(pos@rotation, 90, tolerance = 1e-9)
  expect_equal(pos@left, 0)
  expect_equal(pos@top, 0)
  expect_equal(pos@width, 2, tolerance = 1e-9)
  expect_equal(pos@height, 1, tolerance = 1e-9)
})

test_that("position_from_raw() handles scaled transform (no rotation)", {
  raw <- list(
    size = list(
      width = list(magnitude = 3 * EMU),
      height = list(magnitude = 2 * EMU)
    ),
    transform = list(
      scaleX = 2,
      scaleY = 2,
      shearX = 0,
      shearY = 0,
      translateX = 0,
      translateY = 0
    )
  )
  pos <- position_from_raw(raw)
  expect_equal(pos@width, 6)
  expect_equal(pos@height, 4)
  expect_equal(pos@rotation, 0)
})

test_that("position_from_raw() defaults missing transform fields to identity", {
  raw <- list(
    size = list(
      width = list(magnitude = 1 * EMU),
      height = list(magnitude = 1 * EMU)
    )
  )
  pos <- position_from_raw(raw)
  expect_equal(pos@width, 1)
  expect_equal(pos@height, 1)
  expect_equal(pos@left, 0)
  expect_equal(pos@top, 0)
  expect_equal(pos@rotation, 0)
})

test_that("position_from_raw() normalizes negative rotation into [0, 360)", {
  angle <- -90 * pi / 180
  raw <- list(
    size = list(
      width = list(magnitude = 1 * EMU),
      height = list(magnitude = 1 * EMU)
    ),
    transform = list(
      scaleX = cos(angle),
      scaleY = cos(angle),
      shearX = -sin(angle),
      shearY = sin(angle),
      translateX = 0,
      translateY = 0
    )
  )
  pos <- position_from_raw(raw)
  expect_gte(pos@rotation, 0)
  expect_lt(pos@rotation, 360)
})


# text_runs --------------------------------------------------------------------

test_that("text_runs() returns correct 1-based indices and content for two runs", {
  raw <- list(
    shape = list(
      text = list(
        textElements = list(
          list(
            startIndex = 0,
            endIndex = 5,
            textRun = list(content = "Hello", style = list())
          ),
          list(
            startIndex = 5,
            endIndex = 11,
            textRun = list(content = " world", style = list())
          )
        )
      )
    )
  )
  result <- text_runs(raw)
  expect_equal(nrow(result), 2)
  expect_equal(result$content, c("Hello", " world"))
  expect_equal(result$start, c(1L, 6L))
  expect_equal(result$end, c(5L, 11L))
})

test_that("text_runs() skips paragraphMarker entries", {
  raw <- list(
    shape = list(
      text = list(
        textElements = list(
          list(
            startIndex = 0,
            endIndex = 5,
            textRun = list(content = "Hello", style = list())
          ),
          list(
            startIndex = 5,
            endIndex = 6,
            paragraphMarker = list()
          )
        )
      )
    )
  )
  result <- text_runs(raw)
  expect_equal(nrow(result), 1)
  expect_equal(result$content, "Hello")
})

test_that("text_runs() returns 0-row tibble when no text elements", {
  raw <- list(shape = list(text = list(textElements = list())))
  result <- text_runs(raw)
  expect_equal(nrow(result), 0)
  expect_named(result, c("content", "start", "end", "style"))
})

test_that("text_runs() returns 0-row tibble when shape has no text", {
  raw <- list(shape = list())
  result <- text_runs(raw)
  expect_equal(nrow(result), 0)
})


# text_style_from_api ----------------------------------------------------------

test_that("text_style_from_api() round-trips basic text properties", {
  ts <- text_style(
    bold = TRUE,
    italic = FALSE,
    font_family = "Arial",
    font_size = 14,
    underline = TRUE,
    strikethrough = FALSE,
    small_caps = TRUE,
    baseline_offset = "SUPERSCRIPT"
  )
  result <- text_style_from_api(style = ts@style)

  expect_true(result@bold)
  expect_true(result@underline)
  expect_true(result@small_caps)

  expect_equal(result@font_family, "Arial")
  expect_equal(result@font_size, 14)
  expect_equal(result@baseline_offset, "SUPERSCRIPT")

  expect_false(result@italic)
  expect_false(result@strikethrough)
})

test_that("text_style_from_api() round-trips solid_color text_color", {
  ts <- text_style(text_color = solid_color(c(1, 0, 0)))
  result <- text_style_from_api(style = ts@style)
  expect_equal(result@text_color@red, 1)
  expect_equal(result@text_color@green, 0)
  expect_equal(result@text_color@blue, 0)
})

test_that("text_style_from_api() round-trips theme_color bg_color", {
  ts <- text_style(bg_color = theme_color("ACCENT3"))
  result <- text_style_from_api(style = ts@style)
  expect_true(S7::S7_inherits(result@bg_color, theme_color))
  expect_equal(result@bg_color@theme, "ACCENT3")
})

test_that("text_style_from_api() round-trips paragraph style fields", {
  ts <- text_style(
    alignment = "CENTER",
    line_spacing = 150,
    indent_start = 10,
    indent_end = 5,
    space_above = 6,
    space_below = 3,
    indent_first_line = 12,
    direction = "LEFT_TO_RIGHT",
    spacing_mode = "NEVER_COLLAPSE"
  )
  result <- text_style_from_api(paragraph_style = ts@paragraph_style)
  expect_equal(result@alignment, "CENTER")
  expect_equal(result@line_spacing, 150)
  expect_equal(result@indent_start, 10)
  expect_equal(result@indent_end, 5)
  expect_equal(result@space_above, 6)
  expect_equal(result@space_below, 3)
  expect_equal(result@indent_first_line, 12)
  expect_equal(result@direction, "LEFT_TO_RIGHT")
  expect_equal(result@spacing_mode, "NEVER_COLLAPSE")
})

test_that("text_style_from_api() returns empty text_style for NULL inputs", {
  result <- text_style_from_api()
  expect_null(result@bold)
  expect_null(result@font_family)
  expect_null(result@text_color)
  expect_null(result@alignment)
})
