#' Add image to a Google Slides presentation
#'
#' @description
#' Inserts an image onto a slide. Accepts a ggplot object, a local file path,
#' or a publicly accessible URL.
#'
#' When given a ggplot or local file, the image is temporarily uploaded to
#' Google Drive, inserted into the slide, then deleted from Drive.
#' The image is permanently embedded in the presentation after insertion.
#'
#' @param slide_obj A Google Slides slide object.
#' @param image A ggplot object, a local file path, or a publicly accessible URL string.
#' If an image is specified, it must be in one of the following formats:
#'   * png
#'   * jpg
#'   * gif
#' @param position An object of class `r2slides::slide_position`.
#' @param fit Controls how the image is sized on the slide. One of:
#'   - `"fill"` (default): the image occupies exactly the `position` bounding
#'     box, ignoring aspect ratio. For ggplot objects, the plot is rendered
#'     directly at the target dimensions. For local files, the `magick` package
#'     is required to resize the image before upload.
#'   - `"natural"`: the image is placed at its true pixel dimensions (converted
#'     to inches using `dpi`), anchored to the top-left corner of `position`.
#'     The image may overflow the bounding box. Requires the `magick` package.
#' @param dpi Numeric. Only used when `fit = "natural"`. The DPI to use when
#'   converting pixels to inches. `NULL` auto-detects from the file metadata,
#'   falling back to 96.
#' @param order Optional. One of `"front"` or `"back"`. Controls the Z-order of
#'   the created element. Default: `"front"`.
#'
#' @returns The Google Slides slide object (invisibly).
#' @examples
#' if(FALSE) {
#' plot <- ggplot(mtcars, aes(x = cyl, y = hp)) +
#'            geom_point()
#'
#'   on_slide_number(4) |>
#'     add_image(plot, position = in_top_left())
#' }
#' @export
add_image <- function(
  slide_obj,
  image,
  position,
  fit = c("fill", "natural"),
  dpi = NULL,
  order = c("front", "back")
) {
  fit <- rlang::arg_match(fit)
  order <- rlang::arg_match(order)

  if (!inherits(position, "r2slides::slide_position")) {
    cli::cli_abort(
      "Position must be an object of class {.cls r2slides::slide_position}, not {.cls {class(position)}}."
    )
  }

  # Pre-resize to the exact position dimensions before uploading. Google Slides
  # preserves the original aspect ratio even when a specific size is requested,
  # so we resize client-side first to guarantee the correct proportions.
  if (fit == "fill") {
    fill_path <- preprocess_for_fill(image, position)
    on.exit(unlink(fill_path), add = TRUE)
    image <- fill_path
  }

  resolved <- resolve_image_source(image)

  if (!is.null(resolved$drive_id)) {
    on.exit(
      suppressMessages(try(googledrive::drive_rm(googledrive::as_id(resolved$drive_id)))),
      add = TRUE
    )
  }

  if (isTRUE(resolved$is_temp) && !is.null(resolved$local_path)) {
    on.exit(unlink(resolved$local_path), add = TRUE)
  }

  if (fit == "fill") {
    elem_width_emu  <- position@width_emu
    elem_height_emu <- position@height_emu
  } else {
    rlang::check_installed("magick", reason = "to read image dimensions for fit = 'natural'")
    dim_source <- resolved$local_path %||% resolved$url
    img_dims <- get_image_dims(dim_source)
    resolved_dpi <- dpi %||% img_dims$dpi
    elem_width_emu  <- (img_dims$width_px  / resolved_dpi) * 914400
    elem_height_emu <- (img_dims$height_px / resolved_dpi) * 914400
  }

  obj_id <- paste0(
    "r2img",
    gsub("[^0-9A-Za-z]", "", format(Sys.time(), "%Y%m%d%H%M%OS6"))
  )

  requests <- list(
    list(
      createImage = list(
        objectId = obj_id,
        url = resolved$url,
        elementProperties = list(
          pageObjectId = slide_obj@slide_id,
          size = list(
            width  = list(magnitude = elem_width_emu,  unit = "EMU"),
            height = list(magnitude = elem_height_emu, unit = "EMU")
          ),
          transform = list(
            scaleX     = position@scaleX,
            scaleY     = position@scaleY,
            shearX     = position@shearX,
            shearY     = position@shearY,
            translateX = position@left_emu,
            translateY = position@top_emu,
            unit       = "EMU"
          )
        )
      )
    )
  )

  # Clear crop offsets so the (already correctly sized) image fills the
  # element without any additional aspect-ratio adjustment by Google Slides.
  if (fit == "fill") {
    requests <- c(requests, list(
      list(
        updateImageProperties = list(
          objectId = obj_id,
          imageProperties = list(
            cropProperties = list(
              leftOffset   = 0,
              rightOffset  = 0,
              topOffset    = 0,
              bottomOffset = 0
            )
          ),
          fields = "cropProperties"
        )
      )
    ))
  }

  query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = slide_obj@presentation$presentation_id),
    body = list(requests = requests),
    base = "slides"
  )

  if (order == "back") {
    zorder_by_id(
      presentation_id = slide_obj@presentation$presentation_id,
      element_id = obj_id,
      operation = resolve_zorder_op(order)
    )
  }

  slide_obj@presentation$add_to_ledger(
    element_id = obj_id,
    slide_id = slide_obj@slide_id,
    element_type = "image",
    element_text = NA_character_
  )

  invisible(slide_obj)
}


# Resolves the image argument to list(url, drive_id, local_path).
# drive_id and local_path are NULL when the caller supplied a pre-hosted URL.
resolve_image_source <- function(image, call = rlang::caller_env()) {
  if (inherits(image, "gg")) {
    rlang::check_installed(
      "ggplot2",
      reason = "to render ggplot objects as images"
    )
    tmp <- tempfile(fileext = ".png")
    suppressMessages(ggplot2::ggsave(tmp, plot = image, device = "png"))
    result <- upload_to_drive(tmp, call = call)
    result$local_path <- tmp
    result$is_temp <- TRUE
    result
  } else if (
    is.character(image) && length(image) == 1L && grepl("^https?://", image)
  ) {
    list(url = image, drive_id = NULL, local_path = NULL)
  } else if (is.character(image) && length(image) == 1L) {
    if (!file.exists(image)) {
      cli::cli_abort("File not found: {.path {image}}", call = call)
    }
    result <- upload_to_drive(image, call = call)
    result$local_path <- image
    result
  } else {
    cli::cli_abort(
      c(
        "{.arg image} must be a ggplot object, a local file path, or a URL.",
        "x" = "Got {.cls {class(image)}}."
      ),
      call = call
    )
  }
}


# Uploads a local file to Drive, makes it publicly readable, and returns
# list(url, drive_id) for use with the Slides API.
upload_to_drive <- function(path, call = rlang::caller_env()) {
  mime <- mime_type_from_path(path, call = call)

  drive_file <- suppressMessages(googledrive::drive_upload(
    media = path,
    name = basename(path),
    type = mime
  ))

  file_id <- drive_file$id[[1]]

  # Grant public read access so the Slides API can fetch the image URL.
  # Permission is temporary — the file is deleted after insertion via on.exit().
  req <- gargle::request_build(
    path = paste0("/drive/v3/files/", file_id, "/permissions"),
    method = "POST",
    body = list(role = "reader", type = "anyone"),
    base_url = "https://www.googleapis.com",
    token = r2slides_token()
  )
  gargle::request_make(req)

  list(
    url = paste0("https://drive.google.com/uc?export=view&id=", file_id),
    drive_id = file_id
  )
}


mime_type_from_path <- function(path, call = rlang::caller_env()) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    cli::cli_abort(
      "Unsupported image format: {.val .{ext}}. Supported formats: png, jpg, gif.",
      call = call
    )
  )
}


# Resizes an image to exactly position@width x position@height and saves it
# to a temp PNG. For ggplot objects, ggsave renders at the target size
# directly (no magick required). For local files, magick is used to resize.
preprocess_for_fill <- function(image, position, call = rlang::caller_env()) {
  out <- tempfile(fileext = ".png")

  if (inherits(image, "gg")) {
    rlang::check_installed("ggplot2", reason = "to render ggplot objects as images")
    suppressMessages(ggplot2::ggsave(
      out,
      plot   = image,
      device = "png",
      width  = position@width,
      height = position@height,
      units  = "in",
      dpi    = 300
    ))
  } else {
    rlang::check_installed("magick", reason = "to resize images for fit = 'fill'")
    is_url <- is.character(image) && length(image) == 1L && grepl("^https?://", image)
    if (!is_url && !file.exists(image)) {
      cli::cli_abort("File not found: {.path {image}}", call = call)
    }
    geo <- paste0(round(position@width * 300L), "x", round(position@height * 300L), "!")
    magick::image_write(
      magick::image_resize(magick::image_read(image), geo),
      out,
      format = "png"
    )
  }

  out
}


# Returns list(width_px, height_px, dpi) for a local file path or URL.
get_image_dims <- function(path, call = rlang::caller_env()) {
  rlang::check_installed(
    "magick",
    reason = "to read image dimensions for fit = 'natural'"
  )
  info <- magick::image_info(magick::image_read(path))
  dpi <- suppressWarnings(as.integer(strsplit(info$density[[1]], "x")[[1]][[
    1
  ]]))
  if (is.na(dpi) || dpi <= 0L) dpi <- 96L
  list(
    width_px = as.integer(info$width[[1]]),
    height_px = as.integer(info$height[[1]]),
    dpi = dpi
  )
}


