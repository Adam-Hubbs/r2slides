#' Peek at a slide
#'
#' @description
#' Displays a thumbnail of the current slide
#'
#' @param x A [slide] object.
#' @param ... Ignored; present for S7 generic compatibility.
#' @name peek
#' @export
peek <- S7::new_generic("peek", "x")


#' @param slide The slide object you want to get an image of
#' @rdname peek
#' @export
method(peek, slide) <- function(x, ...) {
  rlang::check_installed("png", reason = 'To show the thumbnail')

  url <- query(
    endpoint = "slides.presentations.pages.getThumbnail",
    params = list(
      presentationId = x@presentation$presentation_id,
      pageObjectId = x@slide_id
    ),
    base = "slides"
  )$contentUrl

  temp_file <- tempfile(fileext = ".png")
  utils::download.file(url, temp_file, mode = "wb", quiet = TRUE)

  plot(1:2, type = 'n', axes = FALSE, xlab = "", ylab = "")
  png::readPNG(temp_file) |>
    graphics::rasterImage(1, 1, 2, 2)

  return(invisible(x))
}
