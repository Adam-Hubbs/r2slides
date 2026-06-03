#' Add an image to a Google Slides presentation
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
#' @param image A ggplot object, a local file path (png, jpg, or gif),
#'   or a publicly accessible URL string.
#' @param position An object of class `r2slides::slide_position`.
#' @param fit Controls how the image maps onto the `position` bounding box.
#'   One of:
#'   - `"contain"` (default): scale to fit entirely within the bounding box,
#'     preserving aspect ratio, centered.
#'   - `"cover"`: fill the bounding box completely, clipping overflow from the
#'     center, preserving aspect ratio.
#'   - `"distort"`: stretch to fill exactly, ignoring aspect ratio.
#'   - `"raw"`: place at the image's natural pixel size at the given `dpi`,
#'     anchored to the top-left of `position`. Overflow is allowed.
#'
#'   All modes except `"distort"` require the `magick` package to read image
#'   dimensions.
#' @param dpi Numeric. Only used when `fit = "raw"`. The DPI to use when
#'   converting pixels to inches. `NULL` auto-detects from the file metadata,
#'   falling back to 96.
#' @param order Optional. One of `"front"` or `"back"`. Controls the Z-order of
#'   the created element. Default: `"front"`.
#'
#' @returns The Google Slides slide object (invisibly).
#'
#' @export
add_image <- function(
  slide_obj,
  image,
  position,
  fit = c("contain", "cover", "distort", "raw"),
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

  resolved <- resolve_image_source(image)

  if (!is.null(resolved$drive_id)) {
    on.exit(
      try(googledrive::drive_rm(googledrive::as_id(resolved$drive_id))),
      add = TRUE
    )
  }

  if (isTRUE(resolved$is_temp) && !is.null(resolved$local_path)) {
    on.exit(unlink(resolved$local_path), add = TRUE)
  }

  dim_source <- resolved$local_path %||% resolved$url
  img_dims <- if (fit == "distort") NULL else get_image_dims(dim_source)

  layout <- compute_image_layout(img_dims, position, fit, dpi)

  emu <- 914400L
  create_image_request <- list(
    requests = list(
      list(
        createImage = list(
          url = resolved$url,
          elementProperties = list(
            pageObjectId = slide_obj@slide_id,
            size = list(
              width = list(
                magnitude = layout$elem_width * emu,
                unit = "EMU"
              ),
              height = list(
                magnitude = layout$elem_height * emu,
                unit = "EMU"
              )
            ),
            transform = list(
              scaleX = position@scaleX,
              scaleY = position@scaleY,
              shearX = position@shearX,
              shearY = position@shearY,
              translateX = layout$elem_left * emu,
              translateY = layout$elem_top * emu,
              unit = "EMU"
            )
          )
        )
      )
    )
  )

  rsp <- query(
    endpoint = "slides.presentations.batchUpdate",
    params = list(presentationId = slide_obj@presentation$presentation_id),
    body = create_image_request,
    base = "slides"
  )

  new_id <- rsp$replies[[1]]$createImage$objectId

  if (!is.null(new_id) && !is.null(layout$crop_props)) {
    update_crop_request <- list(
      requests = list(list(
        updateImageProperties = list(
          objectId = new_id,
          imageProperties = list(cropProperties = layout$crop_props),
          fields = "cropProperties"
        )
      ))
    )
    query(
      endpoint = "slides.presentations.batchUpdate",
      params = list(presentationId = slide_obj@presentation$presentation_id),
      body = update_crop_request,
      base = "slides"
    )
  }

  if (!is.null(new_id) && order == "back") {
    zorder_by_id(
      presentation_id = slide_obj@presentation$presentation_id,
      element_id = new_id,
      operation = resolve_zorder_op(order)
    )
  }

  if (!is.null(new_id)) {
    slide_obj@presentation$add_to_ledger(
      element_id = new_id,
      slide_id = slide_obj@slide_id,
      element_type = "image",
      element_text = NA_character_
    )
  }

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
    ggplot2::ggsave(tmp, plot = image, device = "png")
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

  drive_file <- googledrive::drive_upload(
    media = path,
    name = basename(path),
    type = mime
  )

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


# Returns list(width_px, height_px, dpi) for a local file path or URL.
get_image_dims <- function(path, call = rlang::caller_env()) {
  rlang::check_installed(
    "magick",
    reason = "to read image dimensions for fit modes other than 'distort'"
  )
  info <- magick::image_info(magick::image_read(path))
  dpi <- suppressWarnings(as.integer(strsplit(info$density[[1]], "x")[[1]][[1]]))
  if (is.na(dpi) || dpi <= 0L) dpi <- 96L
  list(
    width_px = as.integer(info$width[[1]]),
    height_px = as.integer(info$height[[1]]),
    dpi = dpi
  )
}


# Computes element placement given image dimensions, a slide_position bounding
# box, and a fit mode. Returns list(elem_left, elem_top, elem_width,
# elem_height, crop_props), all in inches.
compute_image_layout <- function(img_dims, position, fit, dpi = NULL) {
  pos_w <- position@width
  pos_h <- position@height
  pos_l <- position@left
  pos_t <- position@top

  if (fit == "distort") {
    return(list(
      elem_left = pos_l,
      elem_top = pos_t,
      elem_width = pos_w,
      elem_height = pos_h,
      crop_props = NULL
    ))
  }

  img_w <- img_dims$width_px
  img_h <- img_dims$height_px
  img_ratio <- img_w / img_h
  pos_ratio <- pos_w / pos_h

  if (fit == "contain") {
    if (img_ratio > pos_ratio) {
      elem_w <- pos_w
      elem_h <- pos_w / img_ratio
      elem_l <- pos_l
      elem_t <- pos_t + (pos_h - elem_h) / 2
    } else {
      elem_h <- pos_h
      elem_w <- pos_h * img_ratio
      elem_l <- pos_l + (pos_w - elem_w) / 2
      elem_t <- pos_t
    }
    crop_props <- NULL
  } else if (fit == "cover") {
    elem_w <- pos_w
    elem_h <- pos_h
    elem_l <- pos_l
    elem_t <- pos_t

    if (img_ratio > pos_ratio) {
      visible_w <- img_h * pos_ratio
      h_offset <- (img_w - visible_w) / (2 * img_w)
      crop_props <- list(
        leftOffset = h_offset,
        rightOffset = h_offset,
        topOffset = 0,
        bottomOffset = 0
      )
    } else {
      visible_h <- img_w / pos_ratio
      v_offset <- (img_h - visible_h) / (2 * img_h)
      crop_props <- list(
        leftOffset = 0,
        rightOffset = 0,
        topOffset = v_offset,
        bottomOffset = v_offset
      )
    }
  } else if (fit == "raw") {
    resolved_dpi <- dpi %||% img_dims$dpi %||% 96
    elem_w <- img_w / resolved_dpi
    elem_h <- img_h / resolved_dpi
    elem_l <- pos_l
    elem_t <- pos_t
    crop_props <- NULL
  }

  list(
    elem_left = elem_l,
    elem_top = elem_t,
    elem_width = elem_w,
    elem_height = elem_h,
    crop_props = crop_props
  )
}
