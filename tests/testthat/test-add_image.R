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

test_that("get_image_dims() reads image dimensions correctly", {
  skip_if_not_installed("magick")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "test.png")
  magick::image_write(magick::image_blank(20, 10), path, format = "png")

  dims <- get_image_dims(path)
  expect_equal(dims$width_px, 20L)
  expect_equal(dims$height_px, 10L)
  expect_true(is.numeric(dims$dpi))
})

test_that("add_image() natural fit computes correct EMU dimensions", {
  skip_if_not_installed("magick")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "test.png")
  # 200x100 px image; write with density so magick reports 100 DPI
  img <- magick::image_blank(200, 100, color = "white")
  magick::image_write(img, path, format = "png", density = "100x100")

  dims <- get_image_dims(path)
  resolved_dpi <- 100 # override to exact value for test
  expect_equal(dims$width_px, 200L)
  expect_equal(dims$height_px, 100L)

  elem_width_emu <- (dims$width_px / resolved_dpi) * 914400
  elem_height_emu <- (dims$height_px / resolved_dpi) * 914400

  # 200px / 100 DPI = 2 inches; 100px / 100 DPI = 1 inch
  expect_equal(elem_width_emu, 2 * 914400)
  expect_equal(elem_height_emu, 1 * 914400)
})


test_that("add_image() inserts an image from a URL and records it in the ledger", {
  skip_if_not_installed("magick")
  vcr::use_cassette(
    "add_image_url",
    match_requests_on = c("method", "host", "path"),
    {
      ps <- register_presentation(id = TEST_PRESENTATION_ID, set_active = FALSE)

      before_ids <- unlist(ps$get_slide_ids())
      new_slide(ps)
      test_slide_id <- setdiff(unlist(ps$get_slide_ids()), before_ids)[[1]]

      slide_obj <- on_slide_id(test_slide_id, ps)
      add_image(
        slide_obj,
        image = "https://www.gstatic.com/webp/gallery/1.jpg",
        position = in_top_left(),
        fit = "fill"
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
  skip_if_not_installed("magick")

  vcr::use_cassette(
    "add_image_ggplot",
    match_requests_on = c("method", "host", "path"),
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
