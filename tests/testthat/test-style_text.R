# validate_color ---------------------------------------------------------------

test_that("validate_color() returns NULL for all valid inputs", {
  expect_null(validate_color(NULL))
  expect_null(validate_color(c(0, 0, 0)))
  expect_null(validate_color(c(1, 1, 1)))
  expect_null(validate_color(c(0.5, 0.2, 0.9)))
  purrr::walk(theme_colors, ~ expect_null(validate_color(.x)))
})

test_that("validate_color() returns an error string for invalid inputs", {
  expect_snapshot(validate_color(c("r", "g", "b"))) # non-numeric RGB
  expect_snapshot(validate_color(c(-0.1, 0, 0))) # below-range RGB
  expect_snapshot(validate_color(c(0, 1.1, 0))) # above-range RGB
  expect_snapshot(validate_color("NOT_A_THEME")) # invalid theme string
  expect_snapshot(validate_color(c(0.1, 0.2))) # too few elements
  expect_snapshot(validate_color(c(0.1, 0.2, 0.3, 0.4))) # too many elements
})


# text_style construction ------------------------------------------------------

test_that("text_style() constructs with all-NULL defaults", {
  ts <- text_style()
  purrr::walk(
    c(
      "bold",
      "italic",
      "font_family",
      "font_size",
      "text_color",
      "bg_color",
      "link",
      "baseline_offset",
      "small_caps",
      "strikethrough",
      "underline"
    ),
    ~ expect_null(prop(ts, .x))
  )
})

test_that("text_style() stores all valid properties correctly", {
  ts <- text_style(
    bold = TRUE,
    italic = FALSE,
    font_family = "Arial",
    font_size = 12.0,
    text_color = c(1, 0, 0),
    bg_color = "ACCENT1",
    link = "https://example.com",
    baseline_offset = "SUPERSCRIPT",
    small_caps = TRUE,
    strikethrough = FALSE,
    underline = TRUE
  )
  expect_true(ts@bold)
  expect_false(ts@italic)
  expect_equal(ts@font_family, "Arial")
  expect_equal(ts@font_size, 12.0)
  expect_equal(ts@text_color, '#FF0000')
  expect_equal(ts@bg_color, "ACCENT1")
  expect_equal(ts@link, "https://example.com")
  expect_equal(ts@baseline_offset, "SUPERSCRIPT")
  expect_true(ts@small_caps)
  expect_false(ts@strikethrough)
  expect_true(ts@underline)
})

test_that("text_style() rejects invalid logical vector properties", {
  expect_snapshot(error = TRUE, text_style(bold = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, text_style(italic = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, text_style(small_caps = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, text_style(strikethrough = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, text_style(underline = c(TRUE, FALSE)))
})

test_that("text_style() rejects invalid font_family values", {
  expect_snapshot(error = TRUE, text_style(font_family = ""))
  expect_snapshot(error = TRUE, text_style(font_family = c("Arial", "Times")))
})

test_that("text_style() rejects invalid font_size values", {
  expect_snapshot(error = TRUE, text_style(font_size = 0))
  expect_snapshot(error = TRUE, text_style(font_size = -5))
  expect_snapshot(error = TRUE, text_style(font_size = c(12, 14)))
})

test_that("text_style() validates link format", {
  expect_snapshot(error = TRUE, text_style(link = "ftp://example.com"))
  expect_snapshot(error = TRUE, text_style(link = "example.com"))
  expect_snapshot(error = TRUE, text_style(link = ""))
  expect_no_error(text_style(link = "http://example.com"))
  expect_no_error(text_style(link = "https://example.com"))
})

test_that("text_style() validates baseline_offset values", {
  expect_snapshot(error = TRUE, text_style(baseline_offset = "BASELINE"))
  expect_snapshot(error = TRUE, text_style(baseline_offset = "superscript"))
  expect_snapshot(error = TRUE, text_style(baseline_offset = ""))
  expect_no_error(text_style(baseline_offset = "SUPERSCRIPT"))
  expect_no_error(text_style(baseline_offset = "SUBSCRIPT"))
})

test_that("text_style() rejects invalid color values", {
  expect_snapshot(error = TRUE, text_style(bg_color = c(-1, 0, 0)))
  expect_snapshot(error = TRUE, text_style(bg_color = "FAKE_COLOR"))
  expect_snapshot(error = TRUE, text_style(text_color = c(0, 0, 2)))
})


# text_style@style -------------------------------------------------------------

test_that("text_style()@style is empty list when all properties are NULL", {
  expect_equal(text_style()@style, structure(list(), names = character(0)))
})

test_that("text_style()@style maps scalar properties correctly", {
  expect_true(text_style(bold = TRUE)@style$bold)
  expect_false(text_style(italic = FALSE)@style$italic)
  expect_true(text_style(small_caps = TRUE)@style$smallCaps)
  expect_null(text_style(small_caps = TRUE)@style$small_caps) # no snake_case in output
})

test_that("text_style()@style builds correct color structures", {
  expect_equal(
    text_style(text_color = c(1, 0.5, 0))@style$foregroundColor,
    list(opaqueColor = list(rgbColor = list(red = 1, green = 0.502, blue = 0))),
    tolerance = 0.001
  )
  expect_equal(
    text_style(text_color = "ACCENT2")@style$foregroundColor,
    list(opaqueColor = list(themeColor = "ACCENT2"))
  )
  expect_equal(
    text_style(bg_color = c(0, 1, 0))@style$backgroundColor,
    list(opaqueColor = list(rgbColor = list(red = 0, green = 1, blue = 0)))
  )
  expect_equal(
    text_style(bg_color = "DARK1")@style$backgroundColor,
    list(opaqueColor = list(themeColor = "DARK1"))
  )
})

test_that("text_style()@style builds correct fontSize and link structures", {
  expect_equal(
    text_style(font_size = 18)@style$fontSize,
    list(magnitude = 18, unit = "PT")
  )
  expect_equal(
    text_style(link = "https://example.com")@style$link,
    list(url = "https://example.com")
  )
})

test_that("text_style()@style omits NULL properties", {
  ts <- text_style(bold = TRUE)
  expect_named(ts@style, "bold")
})


# text_style@fields ------------------------------------------------------------

test_that("text_style()@fields is character(0) when all properties are NULL", {
  expect_equal(text_style()@fields, character())
})

test_that("text_style()@fields contains all expected camelCase names when fully populated", {
  ts <- text_style(
    bold = TRUE,
    italic = FALSE,
    font_family = "Arial",
    font_size = 12,
    text_color = c(1, 0, 0),
    bg_color = "ACCENT1",
    link = "https://x.com",
    baseline_offset = "SUBSCRIPT",
    small_caps = TRUE,
    strikethrough = TRUE,
    underline = FALSE
  )
  expect_setequal(
    ts@fields,
    c(
      "backgroundColor",
      "foregroundColor",
      "bold",
      "italic",
      "fontFamily",
      "fontSize",
      "link",
      "baselineOffset",
      "smallCaps",
      "strikethrough",
      "underline"
    )
  )
  expect_false(any(grepl("_", ts@fields)))
})

test_that("text_style()@fields only reflects the properties that were set", {
  ts <- text_style(bold = TRUE, font_size = 14)
  expect_setequal(ts@fields, c("bold", "fontSize"))
})


# mush_styles / combine_style_impl ---------------------------------------------

test_that("mush_styles() applies left-priority for overlapping properties and fills from e2", {
  e1 <- text_style(bold = TRUE, font_size = 12)
  e2 <- text_style(bold = FALSE, font_size = 14, italic = TRUE)
  result <- mush_styles(e1, e2)
  expect_true(result@bold) # e1 wins
  expect_equal(result@font_size, 12) # e1 wins
  expect_true(result@italic) # falls through from e2
})

test_that("+ on text_style combines non-overlapping styles", {
  result <- text_style(bold = TRUE) + text_style(italic = TRUE, font_size = 10)
  expect_true(result@bold)
  expect_true(result@italic)
  expect_equal(result@font_size, 10)
})

test_that("combine_style_impl() errors on contradicting field values for all overlapable fields", {
  contradicting_pairs <- list(
    list(text_style(bg_color = c(1, 0, 0)), text_style(bg_color = c(0, 1, 0))),
    list(
      text_style(text_color = c(1, 0, 0)),
      text_style(text_color = c(0, 0, 1))
    ),
    list(text_style(bold = TRUE), text_style(bold = FALSE)),
    list(text_style(italic = TRUE), text_style(italic = FALSE)),
    list(text_style(font_family = "A"), text_style(font_family = "B")),
    list(text_style(font_size = 10), text_style(font_size = 12)),
    list(
      text_style(link = "https://a.com"),
      text_style(link = "https://b.com")
    ),
    list(
      text_style(baseline_offset = "SUPERSCRIPT"),
      text_style(baseline_offset = "SUBSCRIPT")
    ),
    list(text_style(small_caps = TRUE), text_style(small_caps = FALSE)),
    list(text_style(strikethrough = TRUE), text_style(strikethrough = FALSE)),
    list(text_style(underline = TRUE), text_style(underline = FALSE))
  )
  purrr::walk(
    contradicting_pairs,
    ~ {
      expect_snapshot(
        error = TRUE,
        combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
      )
    }
  )
})

test_that("combine_style_impl() allows same-value overlapping fields", {
  result <- combine_style_impl(
    text_style(bold = TRUE, font_size = 12),
    text_style(bold = TRUE, italic = FALSE),
    error_on_contradiction = TRUE
  )
  expect_true(result@bold)
  expect_equal(result@font_size, 12)
  expect_false(result@italic)
})

test_that("combine_style_impl() with error_on_contradiction = FALSE silently picks e1", {
  result <- combine_style_impl(
    text_style(bold = TRUE),
    text_style(bold = FALSE),
    error_on_contradiction = FALSE
  )
  expect_true(result@bold)
})

test_that("combine_style() with .priority = 'first' gives left-priority on conflicts", {
  result <- combine_style(
    text_style(bold = TRUE, font_size = 12),
    text_style(bold = FALSE, italic = TRUE),
    .priority = "first"
  )
  expect_true(result@bold)
  expect_true(result@italic)
  expect_equal(result@font_size, 12)
})

test_that("combine_style() with .priority = 'last' gives right-priority on conflicts", {
  result <- combine_style(
    text_style(bold = TRUE),
    text_style(bold = FALSE, italic = TRUE),
    .priority = "last"
  )
  expect_false(result@bold)
  expect_true(result@italic)
})

test_that("combine_style() with .priority = 'error' errors on contradictions", {
  expect_snapshot(
    error = TRUE,
    combine_style(
      text_style(bold = TRUE),
      text_style(bold = FALSE),
      .priority = "error"
    )
  )
})

test_that("combine_style() errors when non-text_style objects are passed", {
  expect_snapshot(
    error = TRUE,
    combine_style(text_style(bold = TRUE), "not a style")
  )
})


# style_rule construction ------------------------------------------------------

test_that("style_rule() errors without what or default", {
  expect_snapshot(error = TRUE, style_rule())
})

test_that("style_rule() errors on mismatched selector/style counts", {
  expect_snapshot(
    error = TRUE,
    style_rule(
      when = c(grepl("foo", text), grepl("bar", text)),
      what = text_style(bold = TRUE) # 1 style for 2 selectors
    )
  )
})

test_that("style_rule() errors when multiple selectors provided with only default", {
  expect_snapshot(
    error = TRUE,
    style_rule(
      when = c(grepl("foo", text), grepl("bar", text)),
      default = text_style(bold = TRUE)
    )
  )
})

test_that("style_rule() errors when explicit selector given with default but no what", {
  expect_snapshot(
    error = TRUE,
    style_rule(when = grepl("foo", text), default = text_style(bold = TRUE))
  )
})

test_that("style_rule() constructs valid objects in supported configurations", {
  # Single TRUE selector
  sr_simple <- style_rule(when = TRUE, what = text_style(bold = TRUE))
  expect_true(inherits(sr_simple, "r2slides::style_rule"))
  expect_equal(sr_simple@num_selectors, 1L)
  expect_equal(sr_simple@num_styles, 1L)
  expect_false(sr_simple@has_default_style)

  # Default only (implicit TRUE selector)
  sr_default <- style_rule(default = text_style(bold = TRUE))
  expect_true(inherits(sr_default, "r2slides::style_rule"))

  # Predicate + what + default
  sr_with_default <- style_rule(
    when = grepl("x", text),
    what = text_style(bold = TRUE),
    default = text_style(italic = TRUE)
  )
  expect_equal(sr_with_default@num_selectors, 1L)
  expect_equal(sr_with_default@num_styles, 2L)
  expect_true(sr_with_default@has_default_style)

  # Predicate + what, no default
  sr_no_default <- style_rule(
    when = grepl("foo", text),
    what = text_style(bold = TRUE)
  )
  expect_false(sr_no_default@has_default_style)
})

test_that("style_rule() accepts multiple selectors with matching styles", {
  sr <- style_rule(
    when = c(grepl("foo", text), grepl("bar", text)),
    what = list(text_style(bold = TRUE), text_style(italic = TRUE))
  )
  expect_equal(sr@num_selectors, 2L)
  expect_equal(sr@num_styles, 2L)
  expect_false(sr@has_default_style)
})

test_that("+ on style_rule accumulates selectors from both rules", {
  sr1 <- style_rule(when = grepl("foo", text), what = text_style(bold = TRUE))
  sr2 <- style_rule(when = grepl("bar", text), what = text_style(italic = TRUE))
  result <- sr1 + sr2
  expect_equal(result@num_selectors, 2L)
  expect_false(result@has_default_style)
})

test_that("+ on style_rule merges default styles when both sides have a default", {
  sr1 <- style_rule(
    when = grepl("foo", text),
    what = text_style(bold = TRUE),
    default = text_style(font_size = 12)
  )
  sr2 <- style_rule(
    when = grepl("bar", text),
    what = text_style(italic = TRUE),
    default = text_style(underline = TRUE)
  )
  result <- sr1 + sr2
  expect_equal(result@num_selectors, 2L)
  expect_true(result@has_default_style)
  # The merged default should carry properties from both original defaults
  default_style <- result@style[[result@num_styles]]
  expect_equal(default_style@font_size, 12)
  expect_true(default_style@underline)
})

test_that("+ on style_rule preserves single-side default", {
  sr_with <- style_rule(
    when = grepl("foo", text),
    what = text_style(bold = TRUE),
    default = text_style(font_size = 10)
  )
  sr_without <- style_rule(
    when = grepl("bar", text),
    what = text_style(italic = TRUE)
  )

  result1 <- sr_with + sr_without
  expect_true(result1@has_default_style)

  result2 <- sr_without + sr_with
  expect_true(result2@has_default_style)
})


# create_styling_request -------------------------------------------------------

test_that("create_styling_request() errors on unnamed extra vars", {
  sr <- style_rule(when = TRUE, what = text_style(bold = TRUE))
  expect_snapshot(
    error = TRUE,
    create_styling_request(sr, "hello", "e1", "unnamed")
  )
})

test_that("create_styling_request() produces correct request structure for TRUE selector", {
  sr <- style_rule(when = TRUE, what = text_style(bold = TRUE, italic = TRUE))
  result <- create_styling_request(sr, "hello", "my_id")

  expect_type(result, "list")
  expect_length(result, 1L)

  req <- result$updateTextStyle
  expect_equal(req$objectId, "my_id")
  expect_equal(req$textRange$type, "FIXED_RANGE")
  expect_equal(req$textRange$startIndex, 0L) # 0-based
  expect_equal(req$textRange$endIndex, 5L) # nchar("hello") = 5, exclusive

  expect_true(all(c("bold", "italic") %in% strsplit(req$fields, ",")[[1]]))
})

test_that("create_styling_request() returns empty list for FALSE selector without default", {
  sr <- style_rule(when = FALSE, what = text_style(bold = TRUE))
  result <- create_styling_request(sr, "hello", "e1")
  expect_length(result, 0L)
})

test_that("create_styling_request() applies conditional style on match and default on miss", {
  sr <- style_rule(
    when = grepl("hello", text),
    what = text_style(bold = TRUE),
    default = text_style(italic = TRUE)
  )

  result_match <- create_styling_request(sr, "say hello", "e1")
  expect_length(result_match, 1L)
  expect_true(result_match$updateTextStyle$style$bold)
  expect_null(result_match$updateTextStyle$style$italic)

  result_no_match <- create_styling_request(sr, "goodbye", "e1")
  expect_length(result_no_match, 1L)
  expect_true(result_no_match$updateTextStyle$style$italic)
  expect_null(result_no_match$updateTextStyle$style$bold)
})

test_that("create_styling_request() resolves extra vars in the selector data mask", {
  sr <- style_rule(
    when = value > 10,
    what = text_style(bold = TRUE),
    default = text_style(italic = TRUE)
  )

  result_high <- create_styling_request(sr, "text", "e1", value = 15)
  expect_true(result_high$updateTextStyle$style$bold)
  expect_null(result_high$updateTextStyle$style$italic)

  result_low <- create_styling_request(sr, "text", "e1", value = 5)
  expect_true(result_low$updateTextStyle$style$italic)
  expect_null(result_low$updateTextStyle$style$bold)
})

test_that("create_styling_request() gives informative error on missing variable", {
  sr <- style_rule(when = missing_var > 10, what = text_style(bold = TRUE))
  expect_snapshot(
    error = TRUE,
    create_styling_request(sr, "text", "e1")
  )
})

test_that("`create_styling_request()` warns and skips on NA selector result", {
  sr <- style_rule(when = NA, what = text_style(bold = TRUE))
  expect_snapshot(
    result <- create_styling_request(sr, "text", "e1")
  )
  expect_length(result, 0L)
})

test_that("create_styling_request() converts 1-based R indices to 0-based API indices", {
  # Selector returns c(start, end) using 1-based R indexing
  # Wrapping in {} prevents the constructor from splitting on c()
  sr <- style_rule(
    when = {
      m <- regexpr("world", text)
      c(as.integer(m), as.integer(m) + attr(m, "match.length") - 1L)
    },
    what = text_style(bold = TRUE)
  )
  result <- create_styling_request(sr, "hello world", "e1")
  expect_length(result, 1L)
  req <- result$updateTextStyle
  # "world" is at positions 7–11 (1-based) → startIndex=6 (0-based), endIndex=11 (exclusive)
  expect_equal(req$textRange$startIndex, 6L)
  expect_equal(req$textRange$endIndex, 11L)
})

test_that("create_styling_request() skips styling when index selector returns c(0, 0)", {
  sr <- style_rule(
    when = {
      m <- regexpr("missing", text)
      if (m == -1L) {
        c(0L, 0L)
      } else {
        c(as.integer(m), as.integer(m) + attr(m, "match.length") - 1L)
      }
    },
    what = text_style(bold = TRUE)
  )
  result <- create_styling_request(sr, "hello world", "e1")
  expect_length(result, 0L)
})

test_that("create_styling_request() handles multiple selectors, applying each independently", {
  sr <- style_rule(
    when = c(grepl("bold", text), grepl("italic", text)),
    what = list(text_style(bold = TRUE), text_style(italic = TRUE))
  )

  result_both <- create_styling_request(sr, "bold and italic text", "e1")
  result_one <- create_styling_request(sr, "only bold here", "e1")
  result_none <- create_styling_request(sr, "plain text", "e1")

  expect_length(result_both, 2L)
  expect_length(result_one, 1L)
  expect_length(result_none, 0L)
})


# paragraph style properties ---------------------------------------------------

test_that("text_style() defaults all paragraph properties to NULL", {
  ts <- text_style()
  purrr::walk(
    c(
      "alignment",
      "line_spacing",
      "indent_start",
      "indent_end",
      "space_above",
      "space_below",
      "indent_first_line",
      "direction",
      "spacing_mode"
    ),
    ~ expect_null(S7::prop(ts, .x))
  )
})

test_that("text_style() stores valid paragraph property values", {
  ts <- text_style(
    alignment = "CENTER",
    line_spacing = 150,
    indent_start = 18,
    indent_end = 18,
    space_above = 6,
    space_below = 6,
    indent_first_line = 36,
    direction = "LEFT_TO_RIGHT",
    spacing_mode = "NEVER_COLLAPSE"
  )
  expect_equal(ts@alignment, "CENTER")
  expect_equal(ts@line_spacing, 150)
  expect_equal(ts@indent_start, 18)
  expect_equal(ts@indent_end, 18)
  expect_equal(ts@space_above, 6)
  expect_equal(ts@space_below, 6)
  expect_equal(ts@indent_first_line, 36)
  expect_equal(ts@direction, "LEFT_TO_RIGHT")
  expect_equal(ts@spacing_mode, "NEVER_COLLAPSE")
})

test_that("text_style() rejects invalid alignment values", {
  expect_snapshot(error = TRUE, text_style(alignment = "left"))
  expect_snapshot(error = TRUE, text_style(alignment = "UNKNOWN"))
  expect_snapshot(error = TRUE, text_style(alignment = c("CENTER", "END")))
})

test_that("text_style() rejects negative / invalid line_spacing", {
  expect_snapshot(error = TRUE, text_style(line_spacing = -1))
  expect_snapshot(error = TRUE, text_style(line_spacing = c(100, 200)))
})

test_that("text_style() rejects invalid direction values", {
  expect_snapshot(error = TRUE, text_style(direction = "ltr"))
  expect_snapshot(
    error = TRUE,
    text_style(direction = c("LEFT_TO_RIGHT", "RIGHT_TO_LEFT"))
  )
})

test_that("text_style() rejects invalid spacing_mode values", {
  expect_snapshot(error = TRUE, text_style(spacing_mode = "COLLAPSE"))
  expect_snapshot(
    error = TRUE,
    text_style(spacing_mode = c("NEVER_COLLAPSE", "COLLAPSE_LISTS"))
  )
})


# text_style@paragraph_style ---------------------------------------------------

test_that("text_style()@paragraph_style is empty list when all paragraph properties are NULL", {
  expect_equal(
    text_style()@paragraph_style,
    structure(list(), names = character(0))
  )
})

test_that("text_style()@paragraph_style serializes Dimension fields to {magnitude, unit='PT'}", {
  ts <- text_style(space_above = 6, indent_start = 18, indent_first_line = 36)
  expect_equal(ts@paragraph_style$spaceAbove, list(magnitude = 6, unit = "PT"))
  expect_equal(
    ts@paragraph_style$indentStart,
    list(magnitude = 18, unit = "PT")
  )
  expect_equal(
    ts@paragraph_style$indentFirstLine,
    list(magnitude = 36, unit = "PT")
  )
})

test_that("text_style()@paragraph_style passes lineSpacing as a plain number", {
  expect_equal(text_style(line_spacing = 150)@paragraph_style$lineSpacing, 150)
})

test_that("text_style()@paragraph_style passes enum fields as strings", {
  expect_equal(
    text_style(alignment = "JUSTIFIED")@paragraph_style$alignment,
    "JUSTIFIED"
  )
  expect_equal(
    text_style(direction = "RIGHT_TO_LEFT")@paragraph_style$direction,
    "RIGHT_TO_LEFT"
  )
  expect_equal(
    text_style(spacing_mode = "COLLAPSE_LISTS")@paragraph_style$spacingMode,
    "COLLAPSE_LISTS"
  )
})

test_that("text_style()@paragraph_style omits NULL paragraph properties", {
  ts <- text_style(alignment = "CENTER")
  expect_named(ts@paragraph_style, "alignment")
})


# text_style@paragraph_fields --------------------------------------------------

test_that("text_style()@paragraph_fields is character(0) when all paragraph properties are NULL", {
  expect_equal(text_style()@paragraph_fields, character())
})

test_that("text_style()@paragraph_fields contains all expected camelCase names when fully populated", {
  ts <- text_style(
    alignment = "START",
    line_spacing = 100,
    indent_start = 0,
    indent_end = 0,
    space_above = 0,
    space_below = 0,
    indent_first_line = 0,
    direction = "LEFT_TO_RIGHT",
    spacing_mode = "NEVER_COLLAPSE"
  )
  expect_setequal(
    ts@paragraph_fields,
    c(
      "alignment",
      "lineSpacing",
      "indentStart",
      "indentEnd",
      "spaceAbove",
      "spaceBelow",
      "indentFirstLine",
      "direction",
      "spacingMode"
    )
  )
  expect_false(any(grepl("_", ts@paragraph_fields)))
})

test_that("text_style()@paragraph_fields reflects only set paragraph properties", {
  ts <- text_style(alignment = "CENTER", space_above = 6)
  expect_setequal(ts@paragraph_fields, c("alignment", "spaceAbove"))
})


# mush_styles / combine_style_impl with paragraph fields -----------------------

test_that("mush_styles() applies left-priority for overlapping paragraph fields", {
  e1 <- text_style(alignment = "CENTER", space_above = 6)
  e2 <- text_style(alignment = "END", space_above = 12, space_below = 4)
  result <- mush_styles(e1, e2)
  expect_equal(result@alignment, "CENTER") # e1 wins
  expect_equal(result@space_above, 6) # e1 wins
  expect_equal(result@space_below, 4) # falls through from e2
})

test_that("combine_style_impl() errors on contradicting paragraph field values", {
  contradicting_paragraph_pairs <- list(
    list(text_style(alignment = "CENTER"), text_style(alignment = "END")),
    list(text_style(line_spacing = 100), text_style(line_spacing = 200)),
    list(text_style(indent_start = 10), text_style(indent_start = 20)),
    list(text_style(indent_end = 10), text_style(indent_end = 20)),
    list(text_style(space_above = 6), text_style(space_above = 12)),
    list(text_style(space_below = 6), text_style(space_below = 12)),
    list(
      text_style(indent_first_line = 18),
      text_style(indent_first_line = 36)
    ),
    list(
      text_style(direction = "LEFT_TO_RIGHT"),
      text_style(direction = "RIGHT_TO_LEFT")
    ),
    list(
      text_style(spacing_mode = "NEVER_COLLAPSE"),
      text_style(spacing_mode = "COLLAPSE_LISTS")
    )
  )
  purrr::walk(
    contradicting_paragraph_pairs,
    ~ expect_snapshot(
      error = TRUE,
      combine_style_impl(.x[[1]], .x[[2]], error_on_contradiction = TRUE)
    )
  )
})

test_that("combine_style_impl() allows same-value overlapping paragraph fields", {
  result <- combine_style_impl(
    text_style(alignment = "CENTER", space_above = 6),
    text_style(alignment = "CENTER", space_below = 4),
    error_on_contradiction = TRUE
  )
  expect_equal(result@alignment, "CENTER")
  expect_equal(result@space_above, 6)
  expect_equal(result@space_below, 4)
})


# create_styling_request() with paragraph fields --------------------------------

test_that("create_styling_request() emits updateParagraphStyle when paragraph fields are set", {
  sr <- style_rule(
    when = TRUE,
    what = text_style(alignment = "CENTER", space_above = 6)
  )
  result <- create_styling_request(sr, "hello", "my_id")

  expect_length(result, 1L)
  expect_named(result, "updateParagraphStyle")

  para_req <- result$updateParagraphStyle
  expect_equal(para_req$objectId, "my_id")
  expect_equal(para_req$textRange$type, "FIXED_RANGE")
  expect_equal(para_req$style$alignment, "CENTER")
  expect_equal(para_req$style$spaceAbove, list(magnitude = 6, unit = "PT"))
  expect_true(all(
    c("alignment", "spaceAbove") %in% strsplit(para_req$fields, ",")[[1]]
  ))
})

test_that("create_styling_request() emits both updateTextStyle and updateParagraphStyle when both are set", {
  sr <- style_rule(
    when = TRUE,
    what = text_style(bold = TRUE, alignment = "END")
  )
  result <- create_styling_request(sr, "hello", "my_id")

  expect_length(result, 2L)
  request_names <- names(result)
  expect_true("updateTextStyle" %in% request_names)
  expect_true("updateParagraphStyle" %in% request_names)
})

test_that("create_styling_request() emits no updateTextStyle when only paragraph fields are set", {
  sr <- style_rule(
    when = TRUE,
    what = text_style(alignment = "JUSTIFIED")
  )
  result <- create_styling_request(sr, "hello", "my_id")
  request_names <- names(result)
  expect_false("updateTextStyle" %in% request_names)
  expect_true("updateParagraphStyle" %in% request_names)
})

test_that("create_styling_request() default style emits updateParagraphStyle when paragraph fields are set", {
  sr <- style_rule(
    when = grepl("nomatch", text),
    what = text_style(bold = TRUE),
    default = text_style(alignment = "START")
  )
  result <- create_styling_request(sr, "hello", "my_id")
  request_names <- names(result)
  expect_true("updateParagraphStyle" %in% request_names)
  para_req <- result$updateParagraphStyle
  expect_equal(para_req$style$alignment, "START")
})
