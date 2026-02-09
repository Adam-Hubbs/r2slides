test_that("`slide_position` constructor creates valid objects with defaults", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_true(is.slide_position(pos))
  expect_equal(pos@top, 1)
  expect_equal(pos@left, 2)
  expect_equal(pos@width, 3)
  expect_equal(pos@height, 4)
  expect_equal(pos@slide_width, 10)
  expect_equal(pos@slide_height, 5.625)
})

test_that("`slide_position` constructor accepts custom slide_size", {
  pos <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    slide_size = c(7.5, 13.33)
  )

  expect_equal(pos@slide_height, 7.5)
  expect_equal(pos@slide_width, 13.33)
})

test_that("`slide_position` validates top property", {
  expect_snapshot(error = TRUE, {
    slide_position(top = c(1, 2), left = 1, width = 1, height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = -1, left = 1, width = 1, height = 1)
  })
})

test_that("`slide_position` validates left property", {
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = c(1, 2), width = 1, height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = -1, width = 1, height = 1)
  })
})

test_that("`slide_position` validates width property", {
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = c(1, 2), height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 0, height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = -1, height = 1)
  })
})

test_that("`slide_position` validates height property", {
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 1, height = c(1, 2))
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 1, height = 0)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 1, height = -1)
  })
})

test_that("`slide_position` validates slide_width property", {
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 1,
      width = 1,
      height = 1,
      slide_size = c(5.625, c(10, 20))
    )
  })
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 1,
      width = 1,
      height = 1,
      slide_size = c(5.625, 0)
    )
  })
})

test_that("`slide_position` validates slide_height property", {
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 1,
      width = 1,
      height = 1,
      slide_size = c(c(5, 6), 10)
    )
  })
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 1,
      width = 1,
      height = 1,
      slide_size = c(-1, 10)
    )
  })
})

test_that("computed properties work correctly", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_equal(pos@top_end, 5)
  expect_equal(pos@left_end, 5)
  expect_equal(pos@top_pt, 72)
  expect_equal(pos@left_pt, 144)
  expect_equal(pos@width_pt, 216)
  expect_equal(pos@height_pt, 288)
})

test_that("convert_slide_size requires slide_size_old when TRUE", {
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 2,
      width = 3,
      height = 4,
      convert_slide_size = TRUE
    )
  })
})

test_that("convert_slide_size requires slide_size when TRUE", {
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 2,
      width = 3,
      height = 4,
      convert_slide_size = TRUE,
      slide_size_old = c(5.625, 10),
      slide_size = NULL
    )
  })
})

test_that("convert_slide_size validates slide_size_old format", {
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 2,
      width = 3,
      height = 4,
      convert_slide_size = TRUE,
      slide_size_old = c(5.625, 10, 15),
      slide_size = c(7.5, 10)
    )
  })

  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 2,
      width = 3,
      height = 4,
      convert_slide_size = TRUE,
      slide_size_old = "not numeric",
      slide_size = c(7.5, 10)
    )
  })
})

test_that("convert_slide_size validates slide_size format", {
  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 2,
      width = 3,
      height = 4,
      convert_slide_size = TRUE,
      slide_size_old = c(5.625, 10),
      slide_size = c(7.5, 10, 15)
    )
  })

  expect_snapshot(error = TRUE, {
    slide_position(
      top = 1,
      left = 2,
      width = 3,
      height = 4,
      convert_slide_size = TRUE,
      slide_size_old = c(5.625, 10),
      slide_size = "not numeric"
    )
  })
})


test_that("plot method executes without errors", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_invisible(plot(pos))
  expect_invisible(plot(pos, slide_size = c(7.5, 10)))
})

test_that("plot method creates correct visual output", {
  skip_on_ci()

  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  # Test default slide size
  vdiffr::expect_doppelganger(
    "slide_position_default",
    fig = function() plot(pos)
  )

  # Test custom slide size
  vdiffr::expect_doppelganger(
    "slide_position_custom_size",
    fig = function() plot(pos, slide_size = c(7.5, 10))
  )

  # Test position at origin
  pos_origin <- slide_position(top = 0, left = 0, width = 2, height = 2)
  vdiffr::expect_doppelganger(
    "slide_position_origin",
    fig = function() plot(pos_origin)
  )

  # Test position filling most of slide
  pos_large <- slide_position(top = 0.5, left = 1, width = 8, height = 4)
  vdiffr::expect_doppelganger(
    "slide_position_large",
    fig = function() plot(pos_large)
  )

  # Test small position
  pos_small <- slide_position(top = 2, left = 4, width = 0.5, height = 0.5)
  vdiffr::expect_doppelganger(
    "slide_position_small",
    fig = function() plot(pos_small)
  )
})


test_that("convert method to list works", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  result <- convert(pos, to = class_list)

  expect_type(result, "list")
  expect_equal(result$left, 2)
  expect_equal(result$top, 1)
  expect_equal(result$width, 3)
  expect_equal(result$height, 4)
})

test_that("addition with numeric increases width and height", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  pos_new <- pos + 2

  expect_equal(pos_new@top, 1)
  expect_equal(pos_new@left, 2)
  expect_equal(pos_new@width, 5)
  expect_equal(pos_new@height, 6)
})

test_that("addition with numeric works with negative values", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  pos_new <- pos + (-1)

  expect_equal(pos_new@width, 2)
  expect_equal(pos_new@height, 3)
})

test_that("addition of two slide_position objects creates bounding box", {
  pos1 <- slide_position(top = 1, left = 2, width = 3, height = 4)
  pos2 <- slide_position(top = 3, left = 4, width = 2, height = 2)

  result <- pos1 + pos2

  expect_equal(result@top, 1)
  expect_equal(result@left, 2)
  expect_equal(result@left_end, 6)
  expect_equal(result@top_end, 5)
  expect_equal(result@width, 4)
  expect_equal(result@height, 4)
})

test_that("addition of overlapping slide_position objects works", {
  pos1 <- slide_position(top = 1, left = 1, width = 3, height = 3)
  pos2 <- slide_position(top = 2, left = 2, width = 3, height = 3)

  result <- pos1 + pos2

  expect_equal(result@top, 1)
  expect_equal(result@left, 1)
  expect_equal(result@width, 4)
  expect_equal(result@height, 4)
})

test_that("addition fails with incompatible slide sizes", {
  pos1 <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    slide_size = c(5.625, 10)
  )
  pos2 <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    slide_size = c(7.5, 13.33)
  )

  expect_snapshot(error = TRUE, {
    pos1 + pos2
  })
})

test_that("mirror with Horizontal flip works correctly", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  mirrored <- mirror(pos, flip_axis = "Horizontal")

  expect_equal(mirrored@top, 1)
  expect_equal(mirrored@left, 5) # 10 - 2 - 3 = 5
  expect_equal(mirrored@width, 3)
  expect_equal(mirrored@height, 4)
  expect_equal(mirrored@slide_width, 10)
  expect_equal(mirrored@slide_height, 5.625)
})

test_that("mirror with Vertical flip works correctly", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  mirrored <- mirror(pos, flip_axis = "Vertical")

  expect_equal(mirrored@top, 0.625) # 5.625 - 1 - 4 = 0.625
  expect_equal(mirrored@left, 2)
  expect_equal(mirrored@width, 3)
  expect_equal(mirrored@height, 4)
  expect_equal(mirrored@slide_width, 10)
  expect_equal(mirrored@slide_height, 5.625)
})

test_that("mirror preserves custom slide sizes", {
  pos <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    slide_size = c(7.5, 13.33)
  )
  mirrored <- mirror(pos, flip_axis = "Horizontal")

  expect_equal(mirrored@slide_width, 13.33)
  expect_equal(mirrored@slide_height, 7.5)
})

test_that("mirror with default flip_axis argument works", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  mirrored <- mirror(pos)

  # Default should be 'Horizontal'
  expect_equal(mirrored@left, 5)
  expect_equal(mirrored@top, 1)
})

test_that("mirror validates flip_axis argument", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_snapshot(error = TRUE, {
    mirror(pos, flip_axis = "Invalid")
  })
})

test_that("double mirror returns to original position", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  double_horizontal <- mirror(mirror(pos, "Horizontal"), "Horizontal")
  expect_equal(double_horizontal@top, pos@top)
  expect_equal(double_horizontal@left, pos@left)

  double_vertical <- mirror(mirror(pos, "Vertical"), "Vertical")
  expect_equal(double_vertical@top, pos@top)
  expect_equal(double_vertical@left, pos@left)
})

test_that("edge cases: position at origin", {
  pos <- slide_position(top = 0, left = 0, width = 1, height = 1)

  expect_equal(pos@top, 0)
  expect_equal(pos@left, 0)
  expect_equal(pos@top_end, 1)
  expect_equal(pos@left_end, 1)
})

test_that("edge cases: position filling entire slide", {
  pos <- slide_position(
    top = 0,
    left = 0,
    width = 10,
    height = 5.625,
    slide_size = c(5.625, 10)
  )

  expect_equal(pos@left_end, pos@slide_width)
  expect_equal(pos@top_end, pos@slide_height)
})

test_that("edge cases: very small dimensions", {
  pos <- slide_position(top = 0, left = 0, width = 0.01, height = 0.01)

  expect_equal(pos@width, 0.01)
  expect_equal(pos@height, 0.01)
})

test_that("edge cases: large coordinates", {
  pos <- slide_position(
    top = 100,
    left = 200,
    width = 50,
    height = 75,
    slide_size = c(500, 1000)
  )

  expect_equal(pos@top, 100)
  expect_equal(pos@left, 200)
})

test_that("point conversion accuracy", {
  pos <- slide_position(top = 1.5, left = 2.5, width = 3.5, height = 4.5)

  expect_equal(pos@top_pt, 1.5 * 72)
  expect_equal(pos@left_pt, 2.5 * 72)
  expect_equal(pos@width_pt, 3.5 * 72)
  expect_equal(pos@height_pt, 4.5 * 72)
})