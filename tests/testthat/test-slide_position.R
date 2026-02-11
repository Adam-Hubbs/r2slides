# Constructor and basic properties ----

test_that("slide_position constructor creates valid objects with defaults", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_true(is.slide_position(pos))
  expect_equal(pos@top, 1)
  expect_equal(pos@left, 2)
  expect_equal(pos@width, 3)
  expect_equal(pos@height, 4)
  expect_equal(pos@rotation, 0)
  expect_equal(pos@slide_width, 10)
  expect_equal(pos@slide_height, 5.625)
})

test_that("slide_position constructor accepts rotation", {
  pos <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    rotation = 45
  )

  expect_equal(pos@rotation, 45)
})

test_that("slide_position constructor accepts custom slide_size", {
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

# Validation tests ----

test_that("slide_position validates numeric properties", {
  # Top validation
  expect_snapshot(error = TRUE, {
    slide_position(top = c(1, 2), left = 1, width = 1, height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = -1, left = 1, width = 1, height = 1)
  })

  # Left validation
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = c(1, 2), width = 1, height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = -1, width = 1, height = 1)
  })

  # Width validation
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = c(1, 2), height = 1)
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 0, height = 1)
  })

  # Height validation
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 1, height = c(1, 2))
  })
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 1, height = 0)
  })

  # Rotation validation
  expect_snapshot(error = TRUE, {
    slide_position(top = 1, left = 1, width = 1, height = 1, rotation = c(1, 2))
  })
})

test_that("slide_position validates slide_size properties", {
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

# Computed properties ----

test_that("computed properties work correctly without rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_equal(pos@top_end, 5)
  expect_equal(pos@left_end, 5)
  expect_equal(pos@top_emu, 914400)
  expect_equal(pos@left_emu, 1828800)
  expect_equal(pos@width_emu, 2743200)
  expect_equal(pos@height_emu, 3657600)
})

test_that("affine transform properties work with no rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 0)

  expect_equal(pos@scaleX, 1)
  expect_equal(pos@scaleY, 1)
  expect_equal(pos@shearX, 0)
  expect_equal(pos@shearY, 0)
  expect_equal(pos@translateX, 2)
  expect_equal(pos@translateY, 1)
})

test_that("affine transform properties work with rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 90)

  expect_equal(pos@scaleX, cos(pi / 2), tolerance = 1e-10)
  expect_equal(pos@scaleY, cos(pi / 2), tolerance = 1e-10)
  expect_equal(pos@shearX, -1, tolerance = 1e-10)
  expect_equal(pos@shearY, 1, tolerance = 1e-10)

    pos2 <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 45)

    expected_cos <- cos(45 * pi / 180)
    expected_sin <- sin(45 * pi / 180)

    expect_equal(pos2@scaleX, expected_cos)
    expect_equal(pos2@scaleY, expected_cos)
    expect_equal(pos2@shearX, -expected_sin)
    expect_equal(pos2@shearY, expected_sin)
})



# Slide size conversion ----

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

test_that("convert_slide_size validates input formats", {
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

# Convert method ----

test_that("convert method to list works without rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  result <- convert(pos, to = class_list)

  expect_type(result, "list")
  expect_equal(result$left, 2)
  expect_equal(result$top, 1)
  expect_equal(result$width, 3)
  expect_equal(result$height, 4)
  expect_null(result$rotation)
})

test_that("convert method to list includes rotation when non-zero", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 45)
  result <- convert(pos, to = class_list)

  expect_type(result, "list")
  expect_equal(result$rotation, 45)
})

# Addition operator ----

test_that("addition with numeric increases width and height", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  pos_new <- pos + 2

  expect_equal(pos_new@top, 1)
  expect_equal(pos_new@left, 2)
  expect_equal(pos_new@width, 5)
  expect_equal(pos_new@height, 6)
  expect_equal(pos_new@rotation, 0)
})

test_that("addition with numeric preserves rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 30)
  pos_new <- pos + 1

  expect_equal(pos_new@rotation, 30)
  expect_equal(pos_new@width, 4)
  expect_equal(pos_new@height, 5)
})

test_that("addition with numeric works with negative values", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  pos_new <- pos + (-1)

  expect_equal(pos_new@width, 2)
  expect_equal(pos_new@height, 3)
})

# Mirror function ----

test_that("mirror with Horizontal flip works correctly", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  mirrored <- mirror(pos, flip_axis = "Horizontal")

  expect_equal(mirrored@top, 1)
  expect_equal(mirrored@left, 5) # 10 - 2 - 3 = 5
  expect_equal(mirrored@width, 3)
  expect_equal(mirrored@height, 4)
  expect_equal(mirrored@rotation, 0)
})

test_that("mirror with Vertical flip works correctly", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  mirrored <- mirror(pos, flip_axis = "Vertical")

  expect_equal(mirrored@top, 0.625) # 5.625 - 1 - 4 = 0.625
  expect_equal(mirrored@left, 2)
  expect_equal(mirrored@width, 3)
  expect_equal(mirrored@height, 4)
  expect_equal(mirrored@rotation, 0)
})

test_that("mirror negates rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 30)

  mirrored_h <- mirror(pos, flip_axis = "Horizontal")
  expect_equal(mirrored_h@rotation, -30)

  mirrored_v <- mirror(pos, flip_axis = "Vertical")
  expect_equal(mirrored_v@rotation, -30)
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

test_that("mirror validates flip_axis argument", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)

  expect_snapshot(error = TRUE, {
    mirror(pos, flip_axis = "Invalid")
  })
})

test_that("double mirror returns to original position", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 45)

  double_horizontal <- mirror(mirror(pos, "Horizontal"), "Horizontal")
  expect_equal(double_horizontal@top, pos@top)
  expect_equal(double_horizontal@left, pos@left)
  expect_equal(double_horizontal@rotation, pos@rotation)

  double_vertical <- mirror(mirror(pos, "Vertical"), "Vertical")
  expect_equal(double_vertical@top, pos@top)
  expect_equal(double_vertical@left, pos@left)
  expect_equal(double_vertical@rotation, pos@rotation)
})

# Bounding box function ----

test_that("bounding_box works with single unrotated object", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  bb <- bounding_box(pos)

  expect_equal(bb@top, 1)
  expect_equal(bb@left, 2)
  expect_equal(bb@width, 3)
  expect_equal(bb@height, 4)
  expect_equal(bb@rotation, 0)
})

test_that("bounding_box works with single rotated object", {
  # A 4x2 rectangle rotated 90 degrees should become a 2x4 bounding box
  pos <- slide_position(top = 2, left = 2, width = 4, height = 2, rotation = 90)
  bb <- bounding_box(pos)

  # Center is at (4, 3)
  # After 90° rotation, the 4x2 rect becomes 2x4
  # Check that bounding box is larger due to rotation
  expect_equal(bb@width, 2)
  expect_equal(bb@height, 4)
  expect_equal(bb@rotation, 0)
})

test_that("bounding_box works with multiple unrotated objects", {
  pos1 <- slide_position(top = 1, left = 1, width = 2, height = 2)
  pos2 <- slide_position(top = 4, left = 5, width = 2, height = 2)

  bb <- bounding_box(pos1, pos2)

  expect_equal(bb@top, 1)
  expect_equal(bb@left, 1)
  expect_equal(bb@width, 6) # 1 to 7
  expect_equal(bb@height, 5) # 1 to 6
  expect_equal(bb@rotation, 0)
})

test_that("bounding_box works with mixed rotated and unrotated objects", {
  pos1 <- slide_position(top = 1, left = 1, width = 2, height = 2, rotation = 0)
  pos2 <- slide_position(
    top = 1,
    left = 1,
    width = 2,
    height = 2,
    rotation = 45
  )

  bb <- bounding_box(pos1, pos2)

  # Rotated object should extend the bounding box
  expect_gte(bb@width, 2)
  expect_gte(bb@height, 2)
  expect_equal(bb@rotation, 0)
})

test_that("bounding_box requires at least one object", {
  expect_snapshot(error = TRUE,
    bounding_box()
  )
})

test_that("bounding_box validates all inputs are slide_position", {
  pos <- slide_position(top = 1, left = 1, width = 2, height = 2)

  expect_snapshot(error = TRUE,
    bounding_box(pos, "not a position")
  )
})

test_that("bounding_box requires matching slide sizes", {
  pos1 <- slide_position(
    top = 1,
    left = 1,
    width = 2,
    height = 2,
    slide_size = c(5.625, 10)
  )
  pos2 <- slide_position(
    top = 1,
    left = 1,
    width = 2,
    height = 2,
    slide_size = c(7.5, 10)
  )

  expect_snapshot(error = TRUE,
    bounding_box(pos1, pos2)
  )
})

test_that("bounding_box preserves slide_size from input", {
  pos1 <- slide_position(
    top = 1,
    left = 1,
    width = 2,
    height = 2,
    slide_size = c(7.5, 13.33)
  )
  pos2 <- slide_position(
    top = 3,
    left = 3,
    width = 2,
    height = 2,
    slide_size = c(7.5, 13.33)
  )

  bb <- bounding_box(pos1, pos2)

  expect_equal(bb@slide_width, 13.33)
  expect_equal(bb@slide_height, 7.5)
})

# Plot method ----

test_that("plot method executes without errors", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  expect_invisible(plot(pos))
  expect_invisible(plot(pos, slide_size = c(7.5, 10)))
})

test_that("plot method executes with rotation", {
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4, rotation = 45)
  expect_invisible(plot(pos))
})

test_that("plot method creates correct visual output", {
  skip_on_ci()

  # Unrotated positions
  pos <- slide_position(top = 1, left = 2, width = 3, height = 4)
  vdiffr::expect_doppelganger("slide_position_default", fig = function() {
    plot(pos)
  })

  pos_origin <- slide_position(top = 0, left = 0, width = 2, height = 2)
  vdiffr::expect_doppelganger("slide_position_origin", fig = function() {
    plot(pos_origin)
  })

  pos_large <- slide_position(top = 0.5, left = 1, width = 8, height = 4)
  vdiffr::expect_doppelganger("slide_position_large", fig = function() {
    plot(pos_large)
  })

  # Rotated positions
  pos_rot_45 <- slide_position(
    top = 2,
    left = 3,
    width = 3,
    height = 2,
    rotation = 45
  )
  vdiffr::expect_doppelganger("slide_position_rotated_45", fig = function() {
    plot(pos_rot_45)
  })

  pos_rot_90 <- slide_position(
    top = 2,
    left = 3,
    width = 3,
    height = 2,
    rotation = 90
  )
  vdiffr::expect_doppelganger("slide_position_rotated_90", fig = function() {
    plot(pos_rot_90)
  })

  pos_rot_neg <- slide_position(
    top = 2,
    left = 3,
    width = 3,
    height = 2,
    rotation = -30
  )
  vdiffr::expect_doppelganger(
    "slide_position_rotated_negative",
    fig = function() plot(pos_rot_neg)
  )
})

# Edge cases ----

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

test_that("edge cases: 360 degree rotation equals no rotation", {
  pos_0 <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    rotation = 0
  )
  pos_360 <- slide_position(
    top = 1,
    left = 2,
    width = 3,
    height = 4,
    rotation = 360
  )

  expect_equal(pos_0@scaleX, pos_360@scaleX, tolerance = 1e-10)
  expect_equal(pos_0@scaleY, pos_360@scaleY, tolerance = 1e-10)
  expect_equal(pos_0@shearX, pos_360@shearX, tolerance = 1e-10)
  expect_equal(pos_0@shearY, pos_360@shearY, tolerance = 1e-10)
})

test_that("EMU conversion accuracy", {
  pos <- slide_position(top = 1.5, left = 2.5, width = 3.5, height = 4.5)

  expect_equal(pos@top_emu, 1.5 * 914400)
  expect_equal(pos@left_emu, 2.5 * 914400)
  expect_equal(pos@width_emu, 3.5 * 914400)
  expect_equal(pos@height_emu, 4.5 * 914400)
})
