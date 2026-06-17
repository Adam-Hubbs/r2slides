#' Element objects
#'
#' @description
#' S7 classes representing individual content items on a Google Slides slide.
#' Each element holds three pieces of context — the parent `presentation`, the
#' parent `slide`, and the API `element_id` — that allow any generic to fetch
#' the element's raw data on demand without storing a snapshot.
#'
#' The concrete subclasses are:
#' - `text_element` — text boxes (shape type `TEXT_BOX`)
#' - `image_element` — inline images
#' - `chart_element` — linked Google Sheets charts
#' - `table_element` — tables
#' - `shape_element` — other shapes (rectangles, ellipses, etc.)
#'
#' Use [element_type()] to identify the type at runtime, and [get_elements()]
#' or [get_all_elements()] to obtain element objects from a slide.
#'
#' @param presentation A `presentation` R6 object (from [register_presentation()]
#'   or [new_presentation()]).
#' @param slide A `slide` S7 object (from [on_slide_id()] or similar selectors).
#' @param element_id A single character string: the element's Google Slides
#'   object ID.
#'
#' @return An S7 object of the corresponding element class.
#'
#' @seealso [get_elements()], [get_all_elements()], [element_type()],
#'   [get_raw()], [get_position()], [get_text()], [get_text_style()],
#'   [get_image()], [get_linked_sheet()], [get_table()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'
#'   # Retrieve all elements on slide 1
#'   els <- get_all_elements(slide_obj)
#'
#'   # Check the type of the first element
#'   element_type(els[[1]])
#'
#'   # Filter to text elements only
#'   text_els <- get_elements(slide_obj, type = "text")
#' }
#'
#' @export
element <- S7::new_class(
  "element",
  properties = list(
    presentation = S7::new_property(validator = function(value) {
      if (!is.presentation(value)) {
        return("presentation must be of class `presentation`")
      }
    }),

    slide = S7::new_property(
      validator = function(value) {
        if (!is.slide(value)) {
          return("slide must be of class `slide`")
        }
        if (length(value) != 1) {
          return("slide_id must be a single value")
        }
      }
    ),

    element_id = S7::new_property(
      S7::class_character,
      validator = function(value) {
        if (length(value) != 1) {
          "element_id must be a single value"
        }
      }
    )
  )
)

#' @rdname element
#' @export
text_element <- S7::new_class("text_element", parent = element)

#' @rdname element
#' @export
image_element <- S7::new_class("image_element", parent = element)

#' @rdname element
#' @export
chart_element <- S7::new_class("chart_element", parent = element)

#' @rdname element
#' @export
table_element <- S7::new_class("table_element", parent = element)

#' @rdname element
#' @export
shape_element <- S7::new_class("shape_element", parent = element)


#' Get the type of an element
#'
#' @description
#' Returns a single lowercase string identifying the concrete type of an
#' element object. Useful for dispatching on type or for filtering the output
#' of [get_elements()].
#'
#' @param x An `element` object or subclass.
#'
#' @return A single character string: one of `"text"`, `"image"`, `"chart"`,
#'   `"table"`, `"shape"`, or `"unknown"` (the last for the base `element`
#'   class when no known key is present in the raw API node).
#'
#' @seealso [element], [get_elements()]
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   els <- get_all_elements(slide_obj)
#'
#'   purrr::map_chr(els, element_type)
#' }
#'
#' @export
element_type <- S7::new_generic("element_type", "x")

S7::method(element_type, text_element) <- function(x) "text"
S7::method(element_type, image_element) <- function(x) "image"
S7::method(element_type, chart_element) <- function(x) "chart"
S7::method(element_type, table_element) <- function(x) "table"
S7::method(element_type, shape_element) <- function(x) "shape"
S7::method(element_type, element) <- function(x) "unknown"


S7::method(print, element) <- function(x, ...) {
  cli::cli_text("<{element_type(x)}_element>")
  cli::cli_text("Presentation ID: {.val {x@presentation$presentation_id}}")
  cli::cli_text("Slide ID: {.val {x@slide@slide_id}}")
  cli::cli_text("Element ID: {.val {x@element_id}}")
  invisible(x)
}


# Select the right subclass constructor for a raw page element list.
element_class_for <- function(raw) {
  if (!is.null(raw$sheetsChart)) return(chart_element)
  if (!is.null(raw$image)) return(image_element)
  if (!is.null(raw$table)) return(table_element)

  is_text_box <- tryCatch(
    raw$shape$shapeType == "TEXT_BOX",
    error = function(e) FALSE
  )
  if (length(is_text_box) == 0L) is_text_box <- FALSE

  if (isTRUE(is_text_box)) return(text_element)
  if (!is.null(raw$shape)) return(shape_element)

  element
}


#' Get all elements in a slide
#'
#' @description
#' Returns every page element on the slide as a list of typed element objects
#' (`text_element`, `image_element`, etc.). No filtering is applied; use
#' [get_elements()] if you want to filter by type or spatial region.
#'
#' @param slide A `slide` object (e.g. from [on_slide_id()] or [on_slide_number()]).
#'
#' @return A list of element objects. Each entry is one of `text_element`,
#'   `image_element`, `chart_element`, `table_element`, `shape_element`, or the
#'   base `element` class for unrecognised nodes. The list is empty (`list()`)
#'   when the slide has no elements.
#'
#' @seealso [get_elements()] for filtered retrieval, [element_type()] to
#'   inspect individual elements.
#'
#' @examples
#' if (FALSE) {
#'   ps <- register_presentation(id = "YOUR_PRESENTATION_ID", set_active = FALSE)
#'   slide_obj <- on_slide_number(1, ps)
#'   els <- get_all_elements(slide_obj)
#'   length(els)
#' }
#'
#' @export
get_all_elements <- function(slide) {
  purrr::map(slide@elements_raw$pageElements, function(raw) {
    cls <- element_class_for(raw)
    cls(
      presentation = slide@presentation,
      slide = slide,
      element_id = raw$objectId
    )
  })
}


#' Test whether an object is an element
#'
#' @param x An object to test
#'
#' @return `TRUE` if `x` inherits from `element`, `FALSE` otherwise.
#'
#' @export
is_element <- function(x) {
  S7::S7_inherits(x, element)
}
