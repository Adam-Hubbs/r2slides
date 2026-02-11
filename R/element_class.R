#' Element Object
#'
#' An object representing a Google Slides element/slide/presentation combo
#' @param presentation A presentation object
#' @param slide A slide object
#' @param element_id An element ID
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
      class_character,
      validator = function(value) {
        if (length(value) != 1) {
          "element_id must be a single value"
        }
      }
    ),

    raw = S7::new_property(
      getter = function(self) {
        self@slide@elements_raw$pageElements |>
          purrr::keep(\(x) x$objectId == self@element_id) |>
          purrr::pluck(1)
      }
    )
  )
)

#' @rdname element
text_element <- S7::new_class(
  "text_element",
  parent = element,
  properties = list(
    text = S7::new_property(S7::class_character, getter = function(self) {
      self@raw$shape$text$textElements[[2]]$textRun$content
    })
  )
)


method(print, element) <- function(x, ...) {
  cli::cli_text("{.strong Element Object}")
  cli::cli_text("Presentation ID: {.val {x@presentation$presentation_id}}")
  cli::cli_text("Slide ID: {.val {x@slide@slide_id}}")
  cli::cli_text("Element ID: {.val {x@element_id}}")
}


#' Get all elements in a slide
#'
#' @param slide A slide object
#'
#' @return A list of element objects
#'
#' @export
get_all_elements <- function(slide) {
  raw_elems <- slide@elements_raw$pageElements
  purrr::map(raw_elems, function(elem) {
    if (elem_is_text_elem(elem)) {
      text_element(
        presentation = slide@presentation,
        slide = slide,
        element_id = elem$objectId
      )
    } else {
      element(
        presentation = slide@presentation,
        slide = slide,
        element_id = elem$objectId
      )
    }
  })
}


elem_is_text_elem <- function(elem = NULL) {
  if(!is.null(elem)) {
    is_elem <- tryCatch(elem$shape$shapeType == "TEXT_BOX", error = function(e) FALSE)
    if(length(is_elem) == 0) {
      is_elem <- FALSE
    }
  } else {
    is_elem <- FALSE
  }
  is_elem
}