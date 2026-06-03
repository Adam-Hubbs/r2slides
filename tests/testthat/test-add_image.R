test_that("mime_type_from_path() returns correct MIME types", {
  expect_equal(mime_type_from_path("plot.png"), "image/png")
  expect_equal(mime_type_from_path("photo.jpg"), "image/jpeg")
  expect_equal(mime_type_from_path("photo.jpeg"), "image/jpeg")
  expect_equal(mime_type_from_path("anim.gif"), "image/gif")
  expect_equal(mime_type_from_path("IMG.PNG"), "image/png")
})

test_that("mime_type_from_path() errors on unsupported extension", {
  expect_snapshot(error = TRUE, mime_type_from_path("doc.pdf"))
  expect_snapshot(error = TRUE, mime_type_from_path("file.bmp"))
  expect_snapshot(error = TRUE, mime_type_from_path("file.svg"))
  expect_snapshot(error = TRUE, mime_type_from_path("img.webp"))
})

test_that("resolve_image_source() returns NULL drive_id for URLs", {
  result <- resolve_image_source("https://example.com/image.png")
  expect_equal(result$url, "https://example.com/image.png")
  expect_null(result$drive_id)
  expect_null(result$local_path)

  result2 <- resolve_image_source("http://example.com/image.jpg")
  expect_null(result2$drive_id)
})

test_that("resolve_image_source() errors on non-existent file path", {
  expect_snapshot(
    error = TRUE,
    resolve_image_source("/nonexistent/path/image.png")
  )
})

test_that("resolve_image_source() errors on unsupported input type", {
  expect_snapshot(error = TRUE, resolve_image_source(123L))
  expect_snapshot(error = TRUE, resolve_image_source(list()))
  expect_snapshot(error = TRUE, resolve_image_source(c("a", "b")))
})

test_that("add_image() errors on invalid position before any API call", {
  expect_snapshot(
    error = TRUE,
    add_image(
      slide_obj = structure(list(), class = "fake_slide"),
      image = "https://example.com/img.png",
      position = "not_a_position"
    )
  )
})

test_that("get_image_dims() reads PNG dimensions correctly", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "test.png")
  # 20 wide, 10 tall (width_px = 20, height_px = 10)
  png::writePNG(array(1, c(10, 20, 3)), path)

  dims <- get_image_dims(path)
  expect_equal(dims$width_px, 20L)
  expect_equal(dims$height_px, 10L)
  expect_true(is.numeric(dims$dpi))
})

test_that("get_image_dims() reads JPEG dimensions correctly", {
  skip_if_not_installed("jpeg")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "test.jpg")
  # 20 wide, 10 tall
  jpeg::writeJPEG(array(1, c(10, 20, 3)), path)

  dims <- get_image_dims(path)
  expect_equal(dims$width_px, 20L)
  expect_equal(dims$height_px, 10L)
  expect_true(is.numeric(dims$dpi))
})

test_that("compute_image_layout() contain — wider image in square position", {
  # Image: 2:1 ratio (200x100), position: 1:1 (4x4 inches at top=1, left=1)
  pos <- slide_position(top = 1, left = 1, width = 4, height = 4)
  dims <- list(width_px = 200L, height_px = 100L, dpi = 96L)

  layout <- compute_image_layout(dims, pos, "contain", NULL)

  # Image is wider than position, so constrained by width
  expect_equal(layout$elem_width, 4)
  expect_equal(layout$elem_height, 2)
  expect_equal(layout$elem_left, 1)
  # Centered vertically: top = 1 + (4 - 2) / 2 = 2
  expect_equal(layout$elem_top, 2)
  expect_null(layout$crop_props)
})

test_that("compute_image_layout() contain — taller image in landscape position", {
  # Image: 1:2 ratio (100x200), position: 2:1 (4x2 inches at top=0, left=0)
  pos <- slide_position(top = 0, left = 0, width = 4, height = 2)
  dims <- list(width_px = 100L, height_px = 200L, dpi = 96L)

  layout <- compute_image_layout(dims, pos, "contain", NULL)

  # Image is taller than position (pos_ratio=2, img_ratio=0.5), constrained by height
  expect_equal(layout$elem_height, 2)
  expect_equal(layout$elem_width, 1)
  expect_equal(layout$elem_top, 0)
  # Centered horizontally: left = 0 + (4 - 1) / 2 = 1.5
  expect_equal(layout$elem_left, 1.5)
  expect_null(layout$crop_props)
})

test_that("compute_image_layout() cover — wider image in square position", {
  # Image: 2:1 ratio (200x100), position: 1:1 (4x4)
  pos <- slide_position(top = 1, left = 1, width = 4, height = 4)
  dims <- list(width_px = 200L, height_px = 100L, dpi = 96L)

  layout <- compute_image_layout(dims, pos, "cover", NULL)

  expect_equal(layout$elem_width, 4)
  expect_equal(layout$elem_height, 4)
  expect_equal(layout$elem_left, 1)
  expect_equal(layout$elem_top, 1)

  expect_false(is.null(layout$crop_props))
  expect_true(layout$crop_props$leftOffset > 0)
  expect_true(layout$crop_props$rightOffset > 0)
  expect_equal(layout$crop_props$topOffset, 0)
  expect_equal(layout$crop_props$bottomOffset, 0)
})

test_that("compute_image_layout() raw — 100x200 at 100 DPI", {
  pos <- slide_position(top = 0.5, left = 0.5, width = 4, height = 4)
  dims <- list(width_px = 100L, height_px = 200L, dpi = 96L)

  layout <- compute_image_layout(dims, pos, "raw", dpi = 100)

  expect_equal(layout$elem_width, 1)
  expect_equal(layout$elem_height, 2)
  expect_equal(layout$elem_left, 0.5)
  expect_equal(layout$elem_top, 0.5)
  expect_null(layout$crop_props)
})

test_that("compute_image_layout() distort — uses position exactly", {
  pos <- slide_position(top = 1, left = 2, width = 5, height = 3)

  layout <- compute_image_layout(NULL, pos, "distort", NULL)

  expect_equal(layout$elem_width, 5)
  expect_equal(layout$elem_height, 3)
  expect_equal(layout$elem_left, 2)
  expect_equal(layout$elem_top, 1)
  expect_null(layout$crop_props)
})

test_that("resolve_image_source() renders ggplot to PNG and calls upload_to_drive", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(
    data.frame(x = 1:5, y = 1:5),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point()

  upload_called_with <- NULL
  local_mocked_bindings(
    upload_to_drive = function(path, call) {
      upload_called_with <<- path
      list(
        url = "https://drive.google.com/uc?export=view&id=fake123",
        drive_id = "fake123"
      )
    },
    .env = asNamespace("r2slides")
  )

  result <- resolve_image_source(p)

  expect_match(upload_called_with, "\\.png$")
  expect_equal(result$drive_id, "fake123")
  expect_match(result$url, "fake123")
})

test_that("add_image() inserts an image from a URL and records it in the ledger", {
  skip_if(
    !file.exists(testthat::test_path("../fixtures/add_image_url.yml")),
    "cassette not recorded — run interactively after r2slides_auth() to record"
  )
  vcr::use_cassette(
    "add_image_url",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)
      add_image(
        slide_obj,
        image = "https://www.gstatic.com/webp/gallery/1.jpg",
        position = in_top_left()
      )

      delete_slide_raw(ps, test_slide_id)
    }
  )

  elements <- ps$get_elements(element_type = "image")
  expect_false(is.null(elements))
  expect_equal(elements[[1]]$slide_id, test_slide_id)
})

test_that("add_image() with ggplot inserts image (integration)", {
  skip_if_not_installed("ggplot2")
  skip_if(
    !file.exists(testthat::test_path("../fixtures/add_image_ggplot.yml")),
    "cassette not recorded — run interactively after r2slides_auth() to record"
  )

  vcr::use_cassette(
    "add_image_ggplot",
    match_requests_on = c("method", "uri"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)

      p <- ggplot2::ggplot(
        data.frame(x = 1:5, y = 1:5),
        ggplot2::aes(x, y)
      ) +
        ggplot2::geom_point()

      add_image(slide_obj, image = p, position = in_top_left())

      delete_slide_raw(ps, test_slide_id)
    }
  )

  elements <- ps$get_elements(element_type = "image")
  expect_false(is.null(elements))
  expect_equal(elements[[1]]$slide_id, test_slide_id)
})
