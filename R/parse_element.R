EMU_PER_INCH <- 914400

position_from_raw <- function(raw, slide_width = 10, slide_height = 5.625) {
  tr <- raw$transform %||% list()

  scale_x <- tr$scaleX %||% 1
  scale_y <- tr$scaleY %||% 1
  shear_x <- tr$shearX %||% 0
  shear_y <- tr$shearY %||% 0
  translate_x <- tr$translateX %||% 0
  translate_y <- tr$translateY %||% 0

  sx <- sqrt(scale_x^2 + shear_y^2)
  sy <- sqrt(scale_y^2 + shear_x^2)

  rotation_deg <- atan2(shear_y, scale_x) * 180 / pi
  rotation_deg <- rotation_deg %% 360

  width_emu <- (raw$size$width$magnitude %||% 0) * sx
  height_emu <- (raw$size$height$magnitude %||% 0) * sy

  slide_position(
    top = translate_y / EMU_PER_INCH,
    left = translate_x / EMU_PER_INCH,
    width = width_emu / EMU_PER_INCH,
    height = height_emu / EMU_PER_INCH,
    rotation = rotation_deg,
    slide_size = c(slide_height, slide_width)
  )
}

text_runs <- function(raw) {
  empty <- tibble::tibble(
    content = character(),
    start = integer(),
    end = integer(),
    style = list()
  )

  elements <- raw$shape$text$textElements
  if (is.null(elements) || length(elements) == 0) return(empty)

  rows <- purrr::map(elements, function(el) {
    tr <- el$textRun
    if (is.null(tr)) return(NULL)
    tibble::tibble(
      content = tr$content %||% "",
      start = as.integer((el$startIndex %||% 0L) + 1L),
      end = as.integer((el$endIndex %||% 0L)),
      style = list(tr$style %||% list())
    )
  })

  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(empty)
  purrr::list_rbind(rows)
}

text_style_from_api <- function(style = NULL, paragraph_style = NULL) {
  s <- style %||% list()
  p <- paragraph_style %||% list()

  fg_node <- s$foregroundColor
  bg_node <- s$backgroundColor

  text_color <- if (!is.null(fg_node)) color_from_api(fg_node) else NULL
  bg_color <- if (!is.null(bg_node)) color_from_api(bg_node) else NULL

  font_size <- if (!is.null(s$fontSize)) as.double(s$fontSize$magnitude) else
    NULL

  # The API returns "NONE" to indicate no baseline offset; treat as NULL so
  # text_style() validation passes (only "SUPERSCRIPT" and "SUBSCRIPT" are valid).
  baseline_offset_raw <- s$baselineOffset
  baseline_offset <- if (
    !is.null(baseline_offset_raw) && baseline_offset_raw != "NONE"
  ) {
    baseline_offset_raw
  } else {
    NULL
  }

  link <- if (!is.null(s$link)) s$link$url else NULL

  indent_start <- if (!is.null(p$indentStart)) p$indentStart$magnitude else NULL
  indent_end <- if (!is.null(p$indentEnd)) p$indentEnd$magnitude else NULL
  space_above <- if (!is.null(p$spaceAbove)) p$spaceAbove$magnitude else NULL
  space_below <- if (!is.null(p$spaceBelow)) p$spaceBelow$magnitude else NULL
  indent_first_line <- if (!is.null(p$indentFirstLine))
    p$indentFirstLine$magnitude else NULL

  text_style(
    text_color = text_color,
    bg_color = bg_color,
    bold = s$bold,
    italic = s$italic,
    font_family = s$fontFamily,
    font_size = font_size,
    link = link,
    baseline_offset = baseline_offset,
    small_caps = s$smallCaps,
    strikethrough = s$strikethrough,
    underline = s$underline,
    line_spacing = p$lineSpacing,
    alignment = p$alignment,
    indent_start = indent_start,
    indent_end = indent_end,
    space_above = space_above,
    space_below = space_below,
    indent_first_line = indent_first_line,
    direction = p$direction,
    spacing_mode = p$spacingMode
  )
}
