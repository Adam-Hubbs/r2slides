#' Check if two axis-aligned bounding boxes intersect
#'
#' @param a A list with named elements: top, left, bottom, right
#' @param b A list with named elements: top, left, bottom, right
#'
#' @return TRUE if the boxes intersect (overlap), FALSE otherwise
#' @noRd
boxes_overlap <- function(a, b) {
  !(a$right <= b$left ||
    a$left >= b$right ||
    a$bottom <= b$top ||
    a$top >= b$bottom)
}


#' Check if one axis-aligned bounding box is fully contained within another
#'
#' @param outer A list with named elements: top, left, bottom, right
#' @param inner A list with named elements: top, left, bottom, right
#'
#' @return TRUE if inner is fully contained within outer, FALSE otherwise
#' @noRd
box_contains <- function(outer, inner) {
  inner$left >= outer$left &&
    inner$right <= outer$right &&
    inner$top >= outer$top &&
    inner$bottom <= outer$bottom
}


#' Check if a point falls within an axis-aligned bounding box
#'
#' @param box A list with named elements: top, left, bottom, right
#' @param top The point's distance from the top of the slide, in inches
#' @param left The point's distance from the left of the slide, in inches
#'
#' @return TRUE if the point lies inside (or on the edge of) the box
#' @noRd
point_in_box <- function(box, top, left) {
  top >= box$top &&
    top <= box$bottom &&
    left >= box$left &&
    left <= box$right
}


position_to_box <- function(pos) {
  bb <- bounding_box(pos)
  list(
    top = bb@top,
    left = bb@left,
    bottom = bb@top + bb@height,
    right = bb@left + bb@width
  )
}


element_position <- function(raw) {
  transform <- raw$transform
  size <- raw$size

  if (is.null(transform) || is.null(size)) {
    return(NULL)
  }

  width_emu <- size$width$magnitude %||% 0
  height_emu <- size$height$magnitude %||% 0
  tx_emu <- transform$translateX %||% 0
  ty_emu <- transform$translateY %||% 0
  scale_x <- transform$scaleX %||% 1
  scale_y <- transform$scaleY %||% 1

  emu_per_inch <- 914400

  actual_width <- (width_emu * scale_x) / emu_per_inch
  actual_height <- (height_emu * scale_y) / emu_per_inch
  left <- tx_emu / emu_per_inch
  top <- ty_emu / emu_per_inch

  rotation_rad <- transform$shearY %||% 0
  rotation_deg <- asin(max(-1, min(1, rotation_rad))) * 180 / pi

  tryCatch(
    slide_position(
      top = max(0, top),
      left = max(0, left),
      width = max(0.001, actual_width),
      height = max(0.001, actual_height),
      rotation = rotation_deg
    ),
    error = function(e) NULL
  )
}


#' Get elements from a slide
#'
#' @description
#' Retrieves elements from a slide as a list of typed element objects, with
#' optional filtering by element type and/or spatial region.
#'
#' @details
#' **Type filtering** (`type`): Supply one or more of `"text"`, `"image"`,
#' `"chart"`, `"table"`, `"shape"`, `"unknown"` to restrict the result to
#' those element types. Uses [element_type()] for classification.
#'
#' **Spatial filtering** (`within` / `contains`): `within` can be either a
#' region or a single point.
#' - A `slide_position` defines a rectangular region. The `contains` argument
#'   then controls strictness:
#'   - `"any"` (default): keep elements whose bounding box *intersects* the
#'     region (any overlap).
#'   - `"all"`: keep elements whose bounding box is *fully contained* within the
#'     region (strict containment).
#' - A length-2 numeric vector `c(top, left)` in inches defines a single point.
#'   Elements whose bounding box *contains that point* are returned, and
#'   `contains` is ignored.
#'
#' Rotation is accounted for when computing bounding boxes via
#' `slide_position`'s `bounding_box()` computed property.
#'
#' @param slide A `slide` object (from [on_slide_id()], [on_slide_number()],
#'   etc.).
#' @param type Optional character vector of element types to keep. Valid values:
#'   `"text"`, `"image"`, `"chart"`, `"table"`, `"shape"`, `"unknown"`. If
#'   `NULL` (default) all element types are returned.
#' @param within Optional. Either a `slide_position` object (a rectangular
#'   query region) or a numeric vector of length 2 `c(top, left)` in inches (a
#'   single point). For a point, elements whose bounding box contains the point
#'   are returned. If `NULL` (default), no spatial filtering is applied.
#' @param contains One of `"any"` (default) or `"all"`. Controls how an
#'   element's bounding box is compared to a `slide_position` `within` region:
#'   - `"any"`: element is kept if it *overlaps* the region.
#'   - `"all"`: element is kept only if it is *fully inside* the region.
#'
#'   Ignored when `within` is a point.
#'
#' @return A list of element objects (`text_element`, `image_element`, etc.).
#'   Returns an empty list if no elements match the filters.
#'
#' @seealso [get_all_elements()], [element_type()], [get_raw()],
#'   [get_position()], [get_text()], [get_text_style()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'
#'   # All elements
#'   all_els <- get_elements(slide_obj)
#'
#'   # Text elements only
#'   text_els <- get_elements(slide_obj, type = "text")
#'
#'   # Elements overlapping the top-left quadrant
#'   region <- slide_position(top = 0, left = 0, width = 5, height = 2.8)
#'   top_left_els <- get_elements(slide_obj, within = region)
#'
#'   # Elements fully contained within the region
#'   inner_els <- get_elements(slide_obj, within = region, contains = "all")
#'
#'   # Elements covering the point 2" down, 3" from the left
#'   at_point <- get_elements(slide_obj, within = c(2, 3))
#' }
#'
#' @export
get_elements <- function(
  slide,
  type = NULL,
  within = NULL,
  contains = c("any", "all")
) {
  contains <- rlang::arg_match(contains)

  known_types <- c("text", "image", "chart", "table", "shape", "unknown")
  if (!is.null(type)) {
    unknown <- setdiff(type, known_types)
    if (length(unknown) > 0) {
      cli::cli_abort(
        c(
          "{.arg type} contains unknown element type{?s}: {.val {unknown}}",
          "i" = "Valid types are: {.val {known_types}}"
        )
      )
    }
  }

  within_point <- NULL
  if (!is.null(within)) {
    if (is.numeric(within)) {
      if (length(within) != 2) {
        cli::cli_abort(
          c(
            "{.arg within} must be a {.cls slide_position} or a numeric vector of length 2",
            "i" = "Expected {.code c(top, left)} in inches (a point)"
          )
        )
      }
      within_point <- list(top = within[[1]], left = within[[2]])
    } else if (!is.slide_position(within)) {
      cli::cli_abort(
        c(
          "{.arg within} must be a {.cls slide_position} or a numeric vector of length 2",
          "i" = "Expected {.code c(top, left)} in inches (a point)"
        )
      )
    }
  }

  if (!is.slide(slide)) {
    cli::cli_abort("{.arg slide} must be a slide object")
  }

  page_elements <- slide@elements_raw$pageElements

  if (is.null(page_elements) || length(page_elements) == 0) {
    return(list())
  }

  elements <- purrr::map(page_elements, function(raw) {
    cls <- element_class_for(raw)
    cls(
      presentation = slide@presentation,
      slide = slide,
      element_id = raw$objectId
    )
  })

  if (!is.null(type)) {
    elements <- purrr::keep(elements, \(x) element_type(x) %in% type)
  }

  if (!is.null(within)) {
    region_box <- if (is.null(within_point)) position_to_box(within) else NULL

    elements <- purrr::keep(elements, function(elem) {
      raw_elem <- purrr::detect(
        page_elements,
        \(r) r$objectId == elem@element_id
      )
      if (is.null(raw_elem)) {
        return(FALSE)
      }

      pos <- element_position(raw_elem)
      if (is.null(pos)) {
        return(FALSE)
      }

      elem_box <- position_to_box(pos)

      if (!is.null(within_point)) {
        point_in_box(elem_box, within_point$top, within_point$left)
      } else if (contains == "any") {
        boxes_overlap(region_box, elem_box)
      } else {
        box_contains(region_box, elem_box)
      }
    })
  }

  elements
}
