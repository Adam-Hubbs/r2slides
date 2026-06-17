# solid_color construction -----------------------------------------------------

test_that("solid_color() accepts a named R color", {
  sc <- solid_color("red")
  expect_equal(sc@red, 1)
  expect_equal(sc@green, 0)
  expect_equal(sc@blue, 0)
  expect_null(sc@alpha)
})

test_that("solid_color() accepts a hex string", {
  sc <- solid_color("#0000FF")
  expect_equal(sc@red, 0)
  expect_equal(sc@green, 0)
  expect_equal(sc@blue, 1)
})

test_that("solid_color() accepts a numeric c(r, g, b) vector", {
  sc <- solid_color(c(0.5, 0.25, 0.75))
  expect_equal(sc@red, 0.5)
  expect_equal(sc@green, 0.25)
  expect_equal(sc@blue, 0.75)
})

test_that("solid_color() stores alpha when provided", {
  sc <- solid_color("red", alpha = 0.5)
  expect_equal(sc@alpha, 0.5)
})

test_that("solid_color() errors on invalid input", {
  expect_snapshot(error = TRUE, solid_color("NOT_A_COLOR"))
  expect_snapshot(error = TRUE, solid_color(c(0, 0)))
  expect_snapshot(error = TRUE, solid_color(c(1.5, 0, 0)))
})

test_that("solid_color() errors on invalid alpha", {
  expect_snapshot(error = TRUE, solid_color("red", alpha = 1.5))
  expect_snapshot(error = TRUE, solid_color("red", alpha = c(0.1, 0.2)))
})


# theme_color construction -----------------------------------------------------

test_that("theme_color() accepts all valid theme strings", {
  purrr::walk(theme_colors, \(tc) {
    expect_equal(theme_color(tc)@theme, tc)
  })
})

test_that("theme_color() stores alpha when provided", {
  tc <- theme_color("ACCENT1", alpha = 0.3)
  expect_equal(tc@alpha, 0.3)
})

test_that("theme_color() errors on invalid theme string", {
  expect_snapshot(error = TRUE, theme_color("NOT_A_THEME"))
  expect_snapshot(error = TRUE, theme_color(c("ACCENT1", "ACCENT2")))
})


# as_r2s_color ----------------------------------------------------------------

test_that("as_r2s_color() passes through an existing r2s_color unchanged", {
  sc <- solid_color("blue")
  expect_identical(as_r2s_color(sc), sc)

  tc <- theme_color("DARK1")
  expect_identical(as_r2s_color(tc), tc)
})

test_that("as_r2s_color() converts theme strings to theme_color", {
  result <- as_r2s_color("ACCENT1")
  expect_true(S7::S7_inherits(result, theme_color))
  expect_equal(result@theme, "ACCENT1")
})

test_that("as_r2s_color() converts color strings to solid_color", {
  result <- as_r2s_color("red")
  expect_true(S7::S7_inherits(result, solid_color))
  expect_equal(result@red, 1)
})

test_that("as_r2s_color() converts hex strings to solid_color", {
  result <- as_r2s_color("#FF0000")
  expect_true(S7::S7_inherits(result, solid_color))
  expect_equal(result@red, 1)
})


# as_opaque_color_api ----------------------------------------------------------

test_that("as_opaque_color_api() builds rgbColor node from solid_color", {
  sc <- solid_color(c(1, 0.5, 0))
  result <- as_opaque_color_api(sc)
  expect_equal(
    result,
    list(opaqueColor = list(rgbColor = list(red = 1, green = 0.5, blue = 0)))
  )
})

test_that("as_opaque_color_api() builds themeColor node from theme_color", {
  tc <- theme_color("ACCENT2")
  expect_equal(
    as_opaque_color_api(tc),
    list(opaqueColor = list(themeColor = "ACCENT2"))
  )
})

test_that("as_opaque_color_api() warns when alpha is set", {
  sc <- solid_color("red", alpha = 0.5)
  expect_snapshot(as_opaque_color_api(sc))
})


# as_fill_api ------------------------------------------------------------------

test_that("as_fill_api() builds solidFill node without alpha", {
  sc <- solid_color(c(0, 1, 0))
  expect_equal(
    as_fill_api(sc),
    list(
      solidFill = list(
        color = list(rgbColor = list(red = 0, green = 1, blue = 0))
      )
    )
  )
})

test_that("as_fill_api() includes alpha when set", {
  sc <- solid_color("red", alpha = 0.5)
  result <- as_fill_api(sc)
  expect_equal(result$solidFill$alpha, 0.5)
})

test_that("as_fill_api() omits alpha when NULL", {
  sc <- solid_color("red")
  result <- as_fill_api(sc)
  expect_null(result$solidFill$alpha)
})

test_that("as_fill_api() works with theme_color", {
  tc <- theme_color("DARK1")
  expect_equal(
    as_fill_api(tc),
    list(solidFill = list(color = list(themeColor = "DARK1")))
  )
})

test_that("as_fill_api() includes alpha for theme_color when set", {
  tc <- theme_color("ACCENT1", alpha = 0)
  result <- as_fill_api(tc)
  expect_equal(result$solidFill$alpha, 0)
  expect_equal(result$solidFill$color, list(themeColor = "ACCENT1"))
})


# format / print ---------------------------------------------------------------

test_that("format() produces correct string for solid_color", {
  expect_equal(format(solid_color("red")), "<solid_color> #FF0000")
  expect_equal(
    format(solid_color("red", alpha = 0.5)),
    "<solid_color> #FF0000 (alpha: 0.5)"
  )
})

test_that("format() produces correct string for theme_color", {
  expect_equal(format(theme_color("ACCENT1")), "<theme_color> ACCENT1")
  expect_equal(
    format(theme_color("ACCENT1", alpha = 0.3)),
    "<theme_color> ACCENT1 (alpha: 0.3)"
  )
})


# color_from_api — pure round-trips --------------------------------------------

test_that("color_from_api() round-trips solid_color via API node", {
  sc <- solid_color(c(0.5, 0.25, 0.75))
  node <- as_opaque_color_api(sc)
  result <- color_from_api(node)
  expect_true(S7::S7_inherits(result, solid_color))
  expect_equal(result@red, 0.5)
  expect_equal(result@green, 0.25)
  expect_equal(result@blue, 0.75)
})

test_that("color_from_api() round-trips theme_color via API node", {
  tc <- theme_color("ACCENT4")
  node <- as_opaque_color_api(tc)
  result <- color_from_api(node)
  expect_true(S7::S7_inherits(result, theme_color))
  expect_equal(result@theme, "ACCENT4")
})

test_that("color_from_api() defaults missing rgb channels to 0", {
  node <- list(opaqueColor = list(rgbColor = list(red = 1)))
  result <- color_from_api(node)
  expect_equal(result@red, 1)
  expect_equal(result@green, 0)
  expect_equal(result@blue, 0)
})

test_that("color_from_api() returns NULL for NULL input", {
  expect_null(color_from_api(NULL))
})

test_that("color_from_api() returns NULL for empty list input", {
  expect_null(color_from_api(list()))
})


# color_from_api — LIVE: exercises the real API color node shape ───────────────

test_that("color_from_api() correctly parses text_color from a real API text style node", {
  vcr::use_cassette(
    "color_from_api_text_color_live",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before <- unlist(ps$get_slide_ids())
      new_slide(ps)
      slide_id <- setdiff(unlist(ps$get_slide_ids()), before)[[1]]

      # Red text with a blue background fill via text_style
      style <- text_style(
        text_color = solid_color("#FF0000"),
        bg_color = solid_color("#0000FF")
      )
      add_text(
        on_slide_id(slide_id, ps),
        "Color test",
        in_top_left(),
        text_style = style
      )

      el <- get_elements(on_slide_id(slide_id, ps), type = "text")[[1]]
      extracted_style <- get_text_style(el)

      delete_slide_raw(ps, slide_id)
    }
  )

  # The extracted style should carry the red text_color we set.
  # get_text_style() calls text_style_from_api() -> color_from_api() on the
  # real foregroundColor node that Google Slides stores.
  expect_s3_class(extracted_style, "r2slides::text_style")
  expect_false(is.null(extracted_style@text_color))
  expect_true(S7::S7_inherits(extracted_style@text_color, solid_color))
  # Google normalises #FF0000 to red=1, green=0, blue=0
  expect_equal(extracted_style@text_color@red, 1, tolerance = 1e-3)
  expect_equal(extracted_style@text_color@green, 0, tolerance = 1e-3)
  expect_equal(extracted_style@text_color@blue, 0, tolerance = 1e-3)
})
